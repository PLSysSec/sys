{-|

Given information from UninitStatic.hs, this symbolic checker tries to find uninitialized
memory bugs. Specifically, given a possibly uninitialized variable, the checker determines
if the variable's shadow memory is possibly unset (i.e., uninit).

The first part of this file is the straightforward check.

The second is the slightly more complicated false positive suppression. We use shadow
memory to ensure that pointers passed to functions and pointer stored to other pointers
have their shadow bits set, to be conservative.

-}
module Checkers.UninitSymbolic where
import           Checkers.Attack
import           Checkers.Utils.SymexUtils
import           Control.Monad               (forM_, unless, when)
import           Data.List                   (isInfixOf)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as S
import           Data.Word                   (Word32)
import           InternalIR.SimplePath
import           LLVM.AST                    hiding (value)
import qualified LLVM.AST.Constant           as C
import           LLVM.AST.Operand            (CallableOperand)
import           LLVM.AST.ParameterAttribute
import           LLVM.AST.Typed
import           LLVMAST.Interface           hiding (isPointer, isReadOnly)
import           LLVMAST.Interface           (isLocalReference)
import           Prelude                     hiding (max)
import           Symex.Name
import           Symex.Operand
import           Symex.Symex.Symex           (Symex)
import qualified Symex.Symex.Symex           as Sym

--
-- Checker
--

-- | Check if a variable is uninitialized (i.e., if its shadow memory is not set).
uninitRefine :: Name -> Type -> Symex a ()
uninitRefine loadedVar varType = do
  loadedSym <- getDefinedName loadedVar
  loadedType <- Sym.referentTy varType
  shadowResult <- Sym.loadShadow loadedSym loadedType
  notSet <- Sym.zeroConst loadedType
  -- If this query is satisfiable, some shadow bit isn't set, and so is uninitialized
  Sym.isEq loadedType shadowResult notSet >>= Sym.assertBool

--
-- False positive suppression
--

-- | Information about the sizes of variables
data SizeInfo = SizeInfo { sizes   :: M.Map Name Type
                         -- ^ A map between variables and their stack allocated sizes
                         , aliases :: M.Map Name (S.Set Name)
                         -- ^ A map between stack-allocated variables and their aliases
                         }

-- | If op aliases a stack allocated variable, return that variable.
-- Otherwise, return nothing
getStackAllocatedAlias :: Operand -> Symex SizeInfo (Maybe Name)
getStackAllocatedAlias op =
  case op of
    _ | isLocalReference op -> do
      state <- Sym.getAttackerState
      let name = fromJust $ nameOf op
          aliasKey = M.filter (S.member name) $ aliases state
      return $ case M.keys aliasKey of
        [key] -> Just key
        []    -> Nothing
        _     -> error "Malformed aliases in getStackAllocatedAlias"
    _ -> return Nothing

-- | If op is a stack-allocated variable or an alias to one, add name to the set of
-- stack-allocated aliases as well
addStackAllocatedAlias :: Name -> Operand -> Symex SizeInfo ()
addStackAllocatedAlias name op = do
  alias <- getStackAllocatedAlias op
  case alias of
    Just a -> do
      state <- Sym.getAttackerState
      Sym.setAttackerState $
        state { aliases = M.insertWith S.union a (S.fromList [name]) $ aliases state }
    _ -> return ()

-- | Get the maximum size of an alias to a stack-allocated variable, or nothing
-- if the provided op is not an alias
getStackAllocatedAliasSize :: Operand -> Symex SizeInfo (Maybe Word32)
getStackAllocatedAliasSize op = do
  alias <- getStackAllocatedAlias op
  case alias of
    Just name -> do
      state <- Sym.getAttackerState
      let ty = sizes state M.! name
      Sym.maxSizeOf ty >>= return . Just
    _ -> return Nothing

-- | Set shadow memory at op is op is a pointer.
setShadowIfPointer :: Operand -> Symex SizeInfo ()
setShadowIfPointer op = do
  isPointer <- Sym.isPointerTy $ typeOf op
  when isPointer $ do
    maxValSize <- getStackAllocatedAliasSize op
    pointer <- getOperand op
    refTy <- Sym.referentTy $ typeOf op
    case maxValSize of
      -- If we know that op aliases a stack-allocated variable, set shadow with the
      -- size of that stack-allocated variable. This prevents false positives like
      -- the following:
      -- alloca y i64
      -- x = cast y i8
      -- init(x)
      -- result = load y <--- should not be a bug
      Just size -> Sym.setShadow pointer $ IntegerType size
      -- Otherwise, just set shadow to be the size of the referent
      _         -> Sym.setShadow pointer $ refTy


-- | Do each false positive suppression for the uninit memory symbolic checker.
-- The individual suppressions are noted in the comments above each instruction match.
uninitCall :: SimpleInstruction
           -> Symex SizeInfo ()
uninitCall sinstr =

  case getNamedInstr sinstr of

    -- Start tracking stack allocated variables
    Just (name := Alloca ty _ _ _) -> do
      state <- Sym.getAttackerState
      Sym.setAttackerState $ state { sizes   = M.insert name ty $ sizes state
                                   , aliases = M.insert name (S.fromList [name])
                                               $ aliases state
                                   }

    -- If we store a pointer to another pointer, set shadow in the
    -- stored pointer in order to be conservative
    Just (Do (Store _ _ value _ _ _)) -> setShadowIfPointer value

    -- Keep track of things that are aliased to stack allocated variables
    Just (name := BitCast op _ _) -> addStackAllocatedAlias name op
    Just (name := GetElementPtr _ op inds _) ->
      when (all isZero inds) $ addStackAllocatedAlias name op

    -- Set shadow when pointer variables are passed to functions to be conservative
    _ | (Just (Call _ _ _ fname allArgs _ _)) <- getInstr sinstr -> do
      isMemcpy <- doMemcpy sinstr
      isMemset <- doMemset sinstr
      isMemmove <- doMemmove sinstr
      -- Don't do false positive suppression for debug info or for the cases we've handled
      unless ("llvm.lifetime" `isInfixOf` show fname || "llvm.dbg" `isInfixOf` show fname ||
             isMemcpy || isMemset || isMemmove) $ do
        forM_ allArgs $ \argInfo -> do
          let arg         = fst argInfo
              attrs       = snd argInfo
              notReadOnly = not $ isReadOnly attrs
          when (isLocalOperand arg && notReadOnly) $ setShadowIfPointer arg

    _ -> return ()

--
-- Helpers
--

isReadOnly :: [ParameterAttribute] -> Bool
isReadOnly attrs = ReadOnly `elem` attrs || ByVal `elem` attrs

unconstrainUninitCall :: PostAttack SizeInfo
unconstrainUninitCall = PostAttack uninitCall

maybeGetCalledFunName :: CallableOperand -> Maybe Name
maybeGetCalledFunName co = case co of
                             Right (ConstantOperand (C.GlobalReference _ name)) -> Just name
                             _                                                  -> Nothing

