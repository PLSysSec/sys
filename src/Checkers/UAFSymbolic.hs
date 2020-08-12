{-| 

This symbolic checker tries to find UAF bugs. Specifically, the checker marks
the shadow memory on free and checks if any load/store shadow memory is marked.

-}
module Checkers.UAFSymbolic where
import           Checkers.Attack
import           Control.Monad               (forM_, when, unless)
import           InternalIR.SimplePath
import           LLVM.AST                    hiding (value, args)
import qualified LLVM.AST.Constant           as C
import           LLVM.AST.Typed
import           LLVMAST.Interface           hiding (isPointer, isReadOnly)
import           Data.List                   (isInfixOf, isSuffixOf)
import           Prelude                     hiding (max)
import           Symex.Name
import           Symex.Operand
import           Symex.Symex.Symex           (Symex)
import qualified Symex.Symex.Symex           as Sym

import           Data.ByteString.UTF8 (toString)
import           Data.ByteString.Short (fromShort)

--
-- Checker
--

-- | Checking BSD or something else?
checkingBSD :: Bool
checkingBSD = True

-- | The post hook attack exposed
uafPostAttack :: Type -> PostAttack a
uafPostAttack ty = PostAttack $ uafAttack ty

-- | Set shadow memory at op if op is a pointer.
setShadowIfPointer :: Operand -> Type -> Symex a ()
setShadowIfPointer op ty = do
  isPointer <- Sym.isPointerTy $ typeOf op
  when isPointer $ do pointer <- getOperand op
                      refTy <- Sym.referentTy $ ty 
                      Sym.setShadow pointer $ refTy

-- | Set shadow memory at op if op is a pointer.
setShadowIfPointerWithLen :: Operand -> Operand -> Type -> Symex a ()
setShadowIfPointerWithLen op (ConstantOperand (C.Int _ len)) _ = do
  isPointer <- Sym.isPointerTy $ typeOf op
  when isPointer $ do pointer <- getOperand op
                      Sym.setShadowBits pointer $ fromInteger len
setShadowIfPointerWithLen op _ ty = setShadowIfPointer op ty

-- | Check the shadow memory at op and assert its not marked (freed).
ensureShadowNotSet :: Operand -> Symex a ()
ensureShadowNotSet op = do
  case nameOf op of
    Nothing -> return ()
    Just name -> do sym <- getDefinedName name
                    ty <- Sym.referentTy $ typeOf op
                    shadowResult <- Sym.loadShadow sym ty
                    Sym.anyBitsSet shadowResult >>= Sym.assertBool

-- | Check the shadow memory at op and assert its not marked (freed).
ensureShadowNotSetWithLen :: Operand -> Operand -> Symex a ()
ensureShadowNotSetWithLen op (ConstantOperand (C.Int _ len)) = do
  case nameOf op of
    Nothing -> return ()
    Just name -> do sym <- getDefinedName name
                    shadowResult <- Sym.loadShadowBits sym $ fromInteger len
                    -- If this query is satisfiable, some shadow bit is set, and so is freed
                    Sym.anyBitsSet shadowResult >>= Sym.assertBool
ensureShadowNotSetWithLen op _ = ensureShadowNotSet op


-- | Do each false positive suppression for the uninit memory symbolic checker.
uafAttack :: Type -> SimpleInstruction -> Symex a ()
uafAttack ty sinstr = do
  case getNamedInstr sinstr of
    -- Set shadow bits of freed thing
    Just (Do (Call _ _ _ (Right func) ((op, _):(len, _):_) _ _)) | isContigFree func -> do
       -- check if already freed
       ensureShadowNotSetWithLen op len
       -- free
       setShadowIfPointerWithLen op len ty
    Just (Do (Call _ _ _ (Right func) ((op, _):_) _ _)) | isFree func -> do
        -- check if already freed
       ensureShadowNotSet op
       -- free
       setShadowIfPointer op ty

    -- If this is a load/store/call check the shadow bits
    Just (Do (Store _ addr _ _ _ _)) -> ensureShadowNotSet addr
    Just (_ := Load _ addr _ _ _)    -> ensureShadowNotSet addr

    _ | (Just (Call _ _ _ (Right func) args _ _)) <- getInstr sinstr -> do
      unless (isLLVMLifetimeFunc func) $ forM_ args $ \(arg, _) -> do
        isPointer <- Sym.isPointerTy $ typeOf arg
        when isPointer $ ensureShadowNotSet arg

    _ -> return ()


isLLVMLifetimeFunc :: Operand -> Bool
isLLVMLifetimeFunc op =
  case nameOf op of
    Just fname -> "llvm.lifetime" `isInfixOf` show fname || "llvm.dbg" `isInfixOf` show fname
    _ -> False

isFree :: Operand -> Bool
isFree op = case nameOf op of
          Just (Name fname) | checkingBSD ->
            "free" `isSuffixOf` (toString $ fromShort fname)
          Just (Name fname) -> (toString $ fromShort fname) `elem` freeFuncs
          _ -> False

  where freeFuncs = ["free", "_ZdlPv", "_ZdaPv"] ++ if checkingBSD then ["contigfree"] else []

isContigFree :: Operand -> Bool
isContigFree op = case nameOf op of
    Just (Name fname) | checkingBSD -> (toString $ fromShort fname) `elem` ["contigfree"]
    _ -> False
