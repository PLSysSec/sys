{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|

Translate an LLVM constant into a symbolic Boolector node.
This module uses the Sys DSL (Symex.Symex.Symex) to translate various constants
into their node forms. Much like constant expressions are conceptually less-complicated
versions of variable expressions, this file is a bit like a less-complicated version
of Symex.hs, which translates whole variable instructions to their node forms.

-}
module Symex.Constant (getConstant, Constant) where
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           InternalIR.PathInfo
import           LLVM.AST.Constant
import           LLVM.AST.Type
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getConstant)
import           Symex.IntegerPredicate
import           Symex.Name
import           Symex.Symex.Symex          as Sym
import           Symex.Symex.Symex          (Node, Symex)

-- | Translate an LLVM constant into a Boolector node
getConstant :: Constant -> Symex a Node
getConstant constant = do
  let constantOperands = getConstantOperands constant
  case constant of

    _ | (Just binOp) <- getBinaryOperator constant -> do
      when (length constantOperands < 2) $ error "Malformed binop"
      operand1 <- getConstant $ constantOperands !! 0
      operand2 <- getConstant $ constantOperands !! 1
      let ty = typeOf constant
      binOp ty operand1 operand2

    _ | (Just castTo) <- getCastKind constant -> do
      when (length constantOperands < 1) $ error "Malformed cast"
      let castVal = constantOperands !! 0
      operand <- getConstant castVal
      castOp castTo operand (typeOf castVal)

    _ | (Just pred) <- getPredicate constant -> do
      when (length constantOperands < 2) $ error "Malformed cmp"
      operand1 <- getConstant $ constantOperands !! 0
      operand2 <- getConstant $ constantOperands !! 1
      let resultTy = typeOf constant
      pred resultTy operand1 operand2

    Int width val -> numberConst (IntegerType width) val
    Null ty -> numberConst ty 0

    Array _ elements -> do
      elementSyms <- mapM getConstant elements
      Sym.aggregate (typeOf constant) elementSyms
    Vector elements -> do
      elementSyms <- mapM getConstant elements
      Sym.aggregate (typeOf constant) elementSyms
    Struct _ _ elements -> do
      elementSyms <- mapM getConstant elements
      Sym.aggregate (typeOf constant) elementSyms
    Undef ty -> freshVar ty
    AggregateZero aggregateTy -> makeZeroConst aggregateTy >>= getConstant

    GlobalReference ty name -> do
      alreadyExists <- nameIsTracked name
      if alreadyExists
      then getDefinedName name
      else do
        pointerVar <- getVariable (makeVarName name) ty
        constants <- programConstants `liftM` Sym.getSymexPathInfo
        valueType <- referentTy ty
        -- Give it a concrete location and mark it as allocated
        pointerLoc <- stackAllocatePointer valueType
        assignVar name pointerLoc
        allocPointer pointerVar
        -- Store the thing at the location
        case M.lookup name constants of
          -- local constant (e.g., @x = [2xi8] "hi")
          Just simpleConst -> do
            value <- getConstant simpleConst
            store pointerVar value $ typeOf simpleConst
          -- extern constant (e.g., @stderr = extern global file*)
          -- We don't know what it is, so don't constrain it
          _                                -> return ()
        return pointerVar

    GetElementPtr _ addr indecies -> do
      result <- freshVar $ typeOf constant
      addrSym <- getConstant addr
      pointeeType <- referentTy $ typeOf addr
      indexSyms <- mapM getConstant indecies
      doGep result addrSym pointeeType indexSyms

    Select cond trueBr falseBr -> do
      condSym <- getConstant cond
      let condType = typeOf cond
          brType = typeOf trueBr
      trueBrSym <- getConstant trueBr
      falseBrSym <- getConstant falseBr
      selectOp condSym condType brType trueBrSym falseBrSym

    BlockAddress{} -> error "Will never support block address constants probably"
    TokenNone -> error "Don't and will never support token none"
    Float{} -> error "This symex mode does not support floats"
    _ -> error $ unlines $ [ "Unexpected constant type"
                           , show constant
                           ]

-- | Make a constant initialized to all zeros.
makeZeroConst :: Type -> Symex a Constant
makeZeroConst ty =
  case ty of
    IntegerType width -> return $ Int width 0
    PointerType{} -> return $ Null ty
    ArrayType numElems ty -> do
      arrConst <- makeZeroConst ty
      return $ Array ty $ replicate (fromIntegral numElems) arrConst
    VectorType numElems ty -> do
      vectorConst <- makeZeroConst ty
      return $ Vector $ replicate (fromIntegral numElems) vectorConst
    StructureType pack elemTypes -> mapM (\e -> makeZeroConst e) elemTypes
                                    >>= return . Struct Nothing pack
    NamedTypeReference name -> do
      structConst <- getNamedType name >>= \t -> makeZeroConst t
      return $ case structConst of
        Struct _ packed elements -> Struct (Just name) packed elements
        _                        -> structConst
    _ -> error $ unwords ["Cannot make constant from type", show ty]

-- | Given a constant, if it is an LLVM binary operator, return the
-- corresponding Boolector binary operator
getBinaryOperator :: Constant -- ^ Constant from which to extract operator
                  -> Maybe (Type -> Node -> Node -> Symex a Node)
getBinaryOperator constant =
  case constant of
    Add{}  -> Just Sym.addOp
    Sub{}  -> Just Sym.subOp
    Mul{}  -> Just Sym.mulOp
    UDiv{} -> Just Sym.udivOp
    SDiv{} -> Just Sym.sdivOp
    URem{} -> Just Sym.uremOp
    SRem{} -> Just Sym.sremOp
    Shl{}  -> Just Sym.shlOp
    LShr{} -> Just Sym.lshrOp
    AShr{} -> Just Sym.ashrOp
    And{}  -> Just Sym.andOp
    Or{}   -> Just Sym.orOp
    Xor{}  -> Just Sym.xorOp
    _      -> Nothing

-- | Given a constant, if it includes a predicate function, return the given predicate
getPredicate :: Constant -> Maybe (Type -> Node -> Node -> Symex a Node)
getPredicate (ICmp predicate _ _) = Just $ translatePredicate predicate
getPredicate _                    = Nothing

-- | Given an instruction, return the cast kind if it is cast, and nothing otherwise
getCastKind :: Constant -> Maybe CastTo
getCastKind constant =
  case constant of
    Trunc _ ty         -> cast ty TruncKind
    ZExt _ ty          -> cast ty ZExtKind
    SExt _ ty          -> cast ty SExtKind
    PtrToInt _ ty      -> cast ty PtrToIntKind
    IntToPtr _ ty      -> cast ty IntToPtrKind
    BitCast _ ty       -> cast ty BitCastKind
    AddrSpaceCast _ ty -> cast ty UnsupportedKind
    _                  -> Nothing
  where cast ty kind = Just $ CastTo ty kind
