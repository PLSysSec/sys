{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-|

This module provides helper functions for interacting the the
LLVM-pure AST. It appears that LLVM does not provide these
functions, so we do.

-}
module LLVMAST.ASTInterface where
import           Data.Either                 (isLeft)
import           Data.List                   (elem, isInfixOf)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromJust)
import qualified Data.Set                    as S
import           LLVM.AST
import qualified LLVM.AST.Constant           as C
import           LLVM.AST.Global
import qualified LLVM.AST.Operand            as O
import           LLVM.AST.ParameterAttribute
import           LLVM.AST.Typed
import           Prelude                     hiding (elem)

type Path = [Name]

-- Definitions (LLVM.AST)
-- In their words "anything at the top level of a module"

isGlobalDef :: Definition -> Bool
isGlobalDef GlobalDefinition{} = True
isGlobalDef _                  = False

getGlobalDef :: Definition -> Maybe Global
getGlobalDef (GlobalDefinition global) = Just global
getGlobalDef _                         = Nothing

isTypeDef :: Definition -> Bool
isTypeDef TypeDefinition{} = True
isTypeDef _                = False

getTypeDef :: Definition -> Maybe (Name, Maybe Type)
getTypeDef (TypeDefinition name maybeTy) = Just (name, maybeTy)
getTypeDef _                             = Nothing

isMetadataDef :: Definition -> Bool
isMetadataDef MetadataNodeDefinition{} = True
isMetadataDef _                        = False

isNamedMetadataDef :: Definition -> Bool
isNamedMetadataDef NamedMetadataDefinition{} = True
isNamedMetadataDef _                         = False

isInlineAsmDef :: Definition -> Bool
isInlineAsmDef ModuleInlineAssembly{} = True
isInlineAsmDef _                      = False

isFunctionAttrDef :: Definition -> Bool
isFunctionAttrDef FunctionAttributes{} = True
isFunctionAttrDef _                    = False

isComdatDef :: Definition -> Bool
isComdatDef COMDAT{} = True
isComdatDef _        = False

-- Globals (LLVM.AST.Global)

isGlobalVariable :: Global -> Bool
isGlobalVariable GlobalVariable{} = True
isGlobalVariable _                = False

isGlobalAlias :: Global -> Bool
isGlobalAlias GlobalAlias{} = True
isGlobalAlias _             = False

class IsFunction a where
  isFunction :: a -> Bool

instance IsFunction Definition where
  isFunction (GlobalDefinition global) | isFunction global = True
  isFunction _                         = False

instance IsFunction Global where
  isFunction Function{} = True
  isFunction _          = False

-- | For Named things (Named Instruction, Named Terminator, ...), just get the thing itself
withoutName :: Named a -> a
withoutName (_ := x) = x
withoutName (Do x)   = x

-- Basic Blocks (LLVM.AST.Global)

getBlockName :: BasicBlock -> Name
getBlockName (BasicBlock name _ _) = name

getBlockContents :: BasicBlock -> [Named Instruction]
getBlockContents (BasicBlock _ instrs _) = instrs

getBlockTerminator :: BasicBlock -> Named Terminator
getBlockTerminator (BasicBlock _ _ term) = term

getBlockTerminator' :: BasicBlock -> Terminator
getBlockTerminator' (BasicBlock _ _ (Do term))   = term
getBlockTerminator' (BasicBlock _ _ (_ := term)) = term

makeBBMap :: [BasicBlock] -> M.Map Name BasicBlock
makeBBMap bbs = M.fromList $ map (\b -> (getBlockName b, b)) bbs

-- Instructions and operands

class HasOps a where
  getOperands :: a -> [Operand]
  getResultName :: a -> Maybe Name

instance HasOps Instruction where
  getOperands (Add _ _ oper1 oper2 _)               = [oper1, oper2]
  getOperands (FAdd _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (Sub _ _ oper1 oper2 _)               = [oper1, oper2]
  getOperands (FSub _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (Mul _ _ oper1 oper2 _)               = [oper1, oper2]
  getOperands (FMul _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (UDiv _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (SDiv _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (FDiv _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (URem oper1 oper2 _)                  = [oper1, oper2]
  getOperands (SRem oper1 oper2 _)                  = [oper1, oper2]
  getOperands (FRem _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (Shl _ _ oper1 oper2 _)               = [oper1, oper2]
  getOperands (LShr _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (AShr _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (And oper1 oper2 _)                   = [oper1, oper2]
  getOperands (Or oper1 oper2 _)                    = [oper1, oper2]
  getOperands (Xor oper1 oper2 _)                   = [oper1, oper2]
  getOperands (Alloca _ (Just oper) _ _)            = [oper]
  getOperands (Load _ addr _ _ _)                   = [addr]
  getOperands (Store _ addr val _ _ _)              = [addr, val]
  getOperands (GetElementPtr _ addr inds _)         = addr:inds
  getOperands (CmpXchg _ addr expect replace _ _ _) = [addr, expect, replace]
  getOperands (AtomicRMW _ _ addr val _ _)          = [addr, val]
  getOperands (Trunc oper _ _)                      = [oper]
  getOperands (ZExt oper _ _)                       = [oper]
  getOperands (SExt oper _ _)                       = [oper]
  getOperands (FPToUI oper _ _)                     = [oper]
  getOperands (FPToSI oper _ _)                     = [oper]
  getOperands (UIToFP oper _ _)                     = [oper]
  getOperands (SIToFP oper _ _)                     = [oper]
  getOperands (FPTrunc oper _ _)                    = [oper]
  getOperands (FPExt oper _ _)                      = [oper]
  getOperands (PtrToInt oper _ _)                   = [oper]
  getOperands (IntToPtr oper _ _)                   = [oper]
  getOperands (BitCast oper _ _)                    = [oper]
  getOperands (AddrSpaceCast oper _ _)              = [oper]
  getOperands (ICmp _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (FCmp _ oper1 oper2 _)                = [oper1, oper2]
  getOperands (Phi _ values _)                      = map fst values
  getOperands (Call _ _ _ _ args' _ _)              = map fst args'
  getOperands (Select cond br1 br2 _)               = [cond, br1, br2]
  getOperands (VAArg arglist _ _)                   = [arglist]
  getOperands (ExtractElement vect ind _)           = [vect, ind]
  getOperands (InsertElement vect elem ind _)       = [vect, elem, ind]
  getOperands (ShuffleVector oper1 oper2 _ _)       = [oper1, oper2]
  getOperands (ExtractValue oper _ _)               = [oper]
  getOperands (InsertValue struct elem _ _)         = [struct, elem]
  getOperands (CatchPad switch args' _)             = switch:args'
  getOperands (CleanupPad parent args' _)           = parent:args'
  getOperands _                                     = []
  getResultName _ = Nothing

instance HasOps Terminator where
  getOperands (Ret (Just result) _) = [result]
  getOperands (CondBr cond _ _ _)   = [cond]
  getOperands (Switch cond _ _ _)   = [cond]
  getOperands (IndirectBr cond _ _) = [cond]
  -- Not including the other terminators for now
  getOperands _                     = []
  getResultName _ = Nothing

instance HasOps (Named Instruction) where
  getOperands instr         = getOperands $ withoutName instr
  getResultName (Do _)      = Nothing
  getResultName (name := _) = Just name

instance HasOps (Named Terminator) where
  getOperands term          = getOperands $ withoutName term
  getResultName (Do _)      = Nothing
  getResultName (name := _) = Just name

instance HasOps BasicBlock where
  getOperands (BasicBlock _ instrs term) = getOperands term ++ concatMap getOperands instrs
  getResultName bb                       = Just $ getBlockName bb

-- Getting a function's Global from higher-level Module constructs
-- Note that if these functions return a Global, it is guaranteed to be a Function

functionFromDefinition :: Definition -> Maybe Global
functionFromDefinition defn =
  case getGlobalDef defn of
    Just global | isFunction global -> Just global
    _                               -> Nothing

functionFromDefinition' :: Definition -> Global
functionFromDefinition' = fromJust . functionFromDefinition

functionFromNameAndModule :: Module -> Name -> Maybe Global
functionFromNameAndModule modAST funcName =
  let modDefns = moduleDefinitions modAST
      pred dfn = getFunctionName dfn == Just funcName
  in  case filter pred modDefns of
        [funcDefn] -> Just $ functionFromDefinition' funcDefn
        []         -> Nothing
        _          -> error $ "functionFromNameAndModule: multiple functions named " ++ show funcName ++ " found in that Module"

-- | LLVM modules may contain Functions that are simply *forward declarations*
-- (prototype declarations). This means that the above functions may return such
-- a declaration, that has no function body. This function exists for the cases
-- where you don't want a forward declaration and are only interested if the
-- function body is actually defined in this module. Currently, we implement
-- this by simply ignoring all functions with no BBs; thisseems to work.
-- Alternately, there may be a more direct way to distinguish forward
-- declarations in LLVM. "linkage = External" is not such a way, however.
definedFunctionFromNameAndModule :: Module -> Name -> Maybe Global
definedFunctionFromNameAndModule modAST funcName =
  case functionFromNameAndModule modAST funcName of
    Nothing                            -> Nothing
    Just f  | null (getFunctionBBs' f) -> Nothing
    x                                  -> x

-- Getting important data about a function from its Global or Definition

class GetFunction a where
  getFunctionName     :: a -> Maybe Name
  getFunctionName'    :: a -> Name
  getFunctionName'    = fromJust . getFunctionName
  getFunctionBBs      :: a -> Maybe [BasicBlock]
  getFunctionBBs'     :: a -> [BasicBlock]
  getFunctionBBs'     = fromJust . getFunctionBBs
  getFunctionParams   :: a -> Maybe ([Parameter], Bool)
  getFunctionParams'  :: a -> ([Parameter], Bool)
  getFunctionParams'  = fromJust . getFunctionParams
  getFunctionRetType  :: a -> Maybe Type
  getFunctionRetType' :: a -> Type
  getFunctionRetType' = fromJust . getFunctionRetType

instance GetFunction Global where
  getFunctionName    f@(Function {}) = Just $ name f
  getFunctionName    _               = Nothing
  getFunctionBBs     f@(Function {}) = Just $ basicBlocks f
  getFunctionBBs     _               = Nothing
  getFunctionParams  f@(Function {}) = Just $ parameters f
  getFunctionParams  _               = Nothing
  getFunctionRetType f@(Function {}) = Just $ returnType f
  getFunctionRetType _               = Nothing

instance GetFunction Definition where
  getFunctionName    defn = functionFromDefinition defn >>= getFunctionName
  getFunctionBBs     defn = functionFromDefinition defn >>= getFunctionBBs
  getFunctionParams  defn = functionFromDefinition defn >>= getFunctionParams
  getFunctionRetType defn = functionFromDefinition defn >>= getFunctionRetType

-- These next three functions basically form an instance of GetFunction for (Module, Name).
-- They return Nothing if no function of that name is found in the Module

bbsFromFunctionName :: Module -> Name -> Maybe [BasicBlock]
bbsFromFunctionName modAST funcName = do
  func <- functionFromNameAndModule modAST funcName
  return $ getFunctionBBs' func

paramsFromFunctionName :: Module -> Name -> Maybe ([Parameter], Bool)
paramsFromFunctionName modAST funcName = do
  func <- functionFromNameAndModule modAST funcName
  return $ getFunctionParams' func

functionRetTypeFromName :: Module -> Name -> Maybe Type
functionRetTypeFromName modAST funcName = do
  func <- functionFromNameAndModule modAST funcName
  return $ getFunctionRetType' func

-- Misc Instruction-related

-- | Given an Instruction (which must be a Call), get the name of the called function.
-- Returns Nothing if the function is inline assembly (and therefore has no Name).
-- Errors if the Instruction is not a Call, or if it's a type of Call we don't yet handle.
nameOfCalledFunc :: Instruction -> Maybe Name
nameOfCalledFunc Call { function = Right (LocalReference _ n) } = Just n
nameOfCalledFunc Call { function = Right (ConstantOperand (C.GlobalReference _ n)) } = Just n
nameOfCalledFunc Call { function = Left _ } = Nothing
nameOfCalledFunc i = error $ "BUG: called nameOfCalledFunc with bad instruction: " ++ show i

-- | Get the names used in operands.
namesOf :: [Operand] -> [Name]
namesOf ops = catMaybes $ map nameOf ops

-- | Get the name used in an operand, if any.
nameOf :: Operand -> Maybe Name
nameOf (LocalReference _ name)                      = Just name
nameOf (ConstantOperand (C.GlobalReference _ name)) = Just name
nameOf _                                            = Nothing

isZero :: Operand -> Bool
isZero (O.ConstantOperand (C.Int _ 0)) = True
isZero _                               = False

nameOf' :: Operand -> Name
nameOf' (LocalReference _ name) = name
nameOf' op                      = error $ "Not a local reference: " ++ show op

-- | Get the blocks this block transitions to
getNextBlocksOf :: BasicBlock -> [Name]
getNextBlocksOf bb = let t = withoutName (getBlockTerminator bb) in case t of
  Ret{}        -> []
  CondBr{}     -> [trueDest t, falseDest t]
  Br{}         -> [dest t]
  Switch{}     -> defaultDest t : map snd (dests t)
  IndirectBr{} -> possibleDests t
  -- Not including the other terminators for now
  _            -> []


getFunName :: Instruction -> Maybe Name
getFunName (Call _ _ _ fun _ _ _) =
  case fun of
    Right (ConstantOperand (C.GlobalReference _ name)) -> Just name
    _                                                  -> Nothing
getFunName _ = Nothing

isCallInstruction :: Named Instruction -> Bool
isCallInstruction (_ := Call {}) = True
isCallInstruction (Do (Call {})) = True
isCallInstruction _              = False

isLocalReference :: Operand -> Bool
isLocalReference O.LocalReference{} = True
isLocalReference _                  = False

isMemCall :: CallableOperand -> Bool
isMemCall co = getCalledFunName co `elem` memCalls

memCalls :: [Name]
memCalls = map mkName memCall

memCall :: [String]
memCall = [ "llvm.memcpy.p0i8.p0i8.i32"
          , "llvm.memcpy.p0i8.p0i8.i64"
          , "llvm.memmove.p0i8.p0i8.i32"
          , "llvm.memmove.p0i8.p0i8.i64"
          , "llvm.memset.p0i8.i32"
          , "llvm.memset.p0i8.i64"
          ]

isStrlen :: CallableOperand -> Bool
isStrlen co = "strlen" `isInfixOf` show (getCalledFunName co)

getCalledFunName :: CallableOperand -> Name
getCalledFunName co = case co of
                        Right (ConstantOperand (C.GlobalReference _ name)) -> name
                        _                                                  -> mkName "unknown"

maybeGetCalledFunName :: CallableOperand -> Maybe Name
maybeGetCalledFunName co = case co of
                             Right (ConstantOperand (C.GlobalReference _ name)) -> Just name
                             _                                                  -> Nothing

getConstant :: Operand -> Maybe Int
getConstant (O.ConstantOperand (C.Int _ val)) = Just $ fromIntegral val
getConstant _                                 = Nothing


isNegative :: Operand -> Bool
isNegative op = if isConst op && not (isUndef op)
                then let opVal = fromJust $ getConst op
                     in case typeOf op of
                          IntegerType 8  -> opVal > 127
                          IntegerType 16 -> opVal > 32767
                          IntegerType 32 -> opVal > 2147483647
                          IntegerType 64 -> opVal > 18446181123756130303
                          _              -> False
                else False

isConst :: Operand -> Bool
isConst O.ConstantOperand{} = True
isConst _                   = False

getConst :: Operand -> Maybe Integer
getConst (O.ConstantOperand (C.Int _ val)) = Just val
getConst _                                 = Nothing

isUndef :: Operand -> Bool
isUndef (O.ConstantOperand (C.Undef _)) = True
isUndef _                               = False

getRefTy' :: Type -> Type
getRefTy' (PointerType ty _) = ty
getRefTy' _                  = error "Not a pointer type"

isInt :: Type -> Bool
isInt IntegerType{} = True
isInt _             = False

getInstruction :: Named Instruction -> Instruction
getInstruction (Do instr)   = instr
getInstruction (_ := instr) = instr

isLLVMAnnot :: CallableOperand -> Bool
isLLVMAnnot n = ("llvm.lifetime" `isInfixOf` show n) ||
                ("llvm.dbg" `isInfixOf` show n)

isReadOnly :: [ParameterAttribute] -> Bool
isReadOnly attrs = ReadOnly `elem` attrs || ByVal `elem` attrs

setDifference :: (Ord a) => S.Set a -> S.Set a -> S.Set a
setDifference set1 set2 = if S.null set1
                          then set2
                          else if S.null set2
                               then set1
                               else S.difference set1 set2

isFloat :: Type -> Bool
isFloat FloatingPointType{} = True
isFloat _                   = False

isAsm :: CallableOperand -> Bool
isAsm = isLeft


isRet :: Terminator -> Bool
isRet Ret{} = True
isRet _     = False

getReferentType :: Type -> Type
getReferentType (PointerType ty _) = ty
getReferentType _                  = error "Called on a non-pointer type"
