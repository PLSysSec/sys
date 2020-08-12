{-|

Given information determined by HeapOOBStatic, this symbolic checker tries to
determine if a heap out-of-bounds acess is possible. Specifically, the static checker
identifies all cases where there is a dependency between the allocation size of
an object and its index size. For example:

x = malloc(y)
z = x[y - 2]

Since almost all such cases are not real bugs, the symbolic checker's job is determine
if the index size (e.g., y-2) can ever be larger than the allocation size (e.g., y).
The first part of this file describes the checker; the second part describes a false
positive suppression that took the checker from 'absolutely useless'--because we drowned
in false positives---to 'pretty useful.'

-}
module Checkers.HeapOOBSymbolic where
import           Checkers.Attack
import           Control.Monad         (unless, when)
import           Data.Maybe            (fromJust, isJust)
import           InternalIR.SimplePath
import           LLVM.AST
import           LLVMAST.Interface
import           Prelude               hiding (either, max)
import           Symex.Name
import           Symex.Symex.Symex     (Node, Symex)
import qualified Symex.Symex.Symex     as Sym

--
-- Checker
--

-- | Determine if an access might be out-of-bounds
mayOOB :: Either Name Int -> Either Name Int -> Int -> Bool -> Symex a ()
mayOOB a i j True  = enforceOOB a i j Sym.isUge
mayOOB a i j False = enforceOOB a i j Sym.isUgt

-- | Given an allocation size of an object, an index size into that object, the
-- size of the 'jump' during indexing, and comparison operation: determine if
-- the index can be larger---by the comparison operation---than the size of the
-- allocation. This indicates an out-of-bounds access of the object
enforceOOB :: Either Name Int
           -> Either Name Int
           -> Int
           -> (Type -> Node -> Node -> Symex a Node)
           -> Symex a ()
-- It was an OOB by a constant, so all we need to do is make sure the path is SAT
enforceOOB Right{} Right{} _ _            = return ()
-- It was an OOB by a variable, so we have to make sure its possible for the
-- access (eIndexSize * jumpSize) to be outside the bounds of the allocation
-- (eAllocSize).
enforceOOB eAllocSize eIndexSize jumpSize cmp = do
  -- Size of the allocation in bytes
  allocSizeSym <- getSymFromEither eAllocSize
  unless (isJust allocSizeSym) $ error "Don't have allocation size"
  castAllocSize <- Sym.castToWidth (fromJust allocSizeSym) 64
  -- Size of the index
  indexSizeSym <- getSymFromEither eIndexSize
  unless (isJust indexSizeSym) $ error "Don't have an index size"
  castIndexSize <- Sym.castToWidth (fromJust indexSizeSym) 64
  -- Size of the index * jump in bytes
  unless (jumpSize `mod` 8 == 0) $ error "Unaligned jump size"
  -- Get the index size in bytes
  finalIndexSize <- makeNumber (jumpSize `div` 8) >>=
                    Sym.mulOp (IntegerType 64) castIndexSize
  -- Make sure the jump is bigger than the allocation
  cmp (IntegerType 64) finalIndexSize castAllocSize >>= Sym.assertBool
  -- Make sure tha malloc is greater than 0
  makeNumber 0 >>= Sym.isUgt (IntegerType 64) castAllocSize >>= Sym.assertBool

makeNumber :: Int -> Symex a Node
makeNumber num = Sym.numberConst (IntegerType 64) $ fromIntegral num

getSymFromEither :: Either Name Int -> Symex a (Maybe Node)
getSymFromEither either = case either of
    Left name -> do
      isTracked <- nameIsTracked name
      if isTracked then getDefinedName name >>= return . Just else return Nothing
    Right num -> makeNumber num >>= return . Just

--
-- False positive suppression
--

-- | Type wrapper around giant-value false positive suppression
innerObj :: PostAttack a
innerObj = PostAttack enforceInnerObj

-- | False positive suppression: Make sure inner fields of objects are 'not giant'
enforceInnerObj :: SimpleInstruction -> Symex a ()
enforceInnerObj instr = case getNamedInstr instr of
  -- Instruction gets an object field
  Just (name := GetElementPtr{}) -> do
    isTracked <- nameIsTracked name
    when isTracked $ do
      ty <- getTrackedNameType name
      refTy <- Sym.referentTy ty

      when (isIntType refTy) $ do
        pointerSym <- getDefinedName name
        valSym <- Sym.load pointerSym refTy
        max <- intmax refTy
        -- Ensure that valSym, the result of loading of the GEP'd pointer, is not giant
        Sym.isUlt refTy valSym max >>= Sym.assertBool

  _ -> return ()

-- | Intmax for each type, used in enforceInnerObj
intmax :: Type -> Symex a Node
intmax ty = case ty of
              IntegerType 8  -> Sym.numberConst ty 127
              IntegerType 16 -> Sym.numberConst ty 400
              IntegerType 32 -> Sym.numberConst ty 2147483647
              IntegerType 64 -> Sym.numberConst ty 2147483647
              _              -> error "Cannot get intmax for non standard type"
