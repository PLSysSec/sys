{-|

This somewhat classic taint checker identifies places where untrusted input
can flow into sensitive operations like memcpy.
There is one twist: if a tainted value is checked, we only untaint it if its
checked against something we don't have information about.

The first part of this file describes the checker state and functions for
manipulating it.

The second part is the taint analysis.

-}

module Checkers.UserInputStatic ( InputBug(..)
                                , InputBasicState
                                , userInputCheck
                                , blankInputState
                                ) where
import           Checkers.Utils.StaticUtils
import           Control.Monad              (forM_, when)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust)
import qualified Data.Set                   as S
import           LLVM.AST
import qualified LLVM.AST.Operand           as O
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getFunName)
import           Static.CheckerState

--
-- Checker state
--

data InputBasicState = InputBasicState { deps    :: M.Map Name (S.Set Name)
                                       , tainted :: S.Set Name
                                       } deriving Show

blankInputState :: InputBasicState
blankInputState = InputBasicState M.empty S.empty

-- | Add a dependency between the given operand and name
addDeps :: Name -> Operand -> Checker InputBasicState b ()
addDeps newAlias (O.LocalReference _ opName) = do
  state <- getState
  let curDeps = deps state
      depSet = case M.lookup opName curDeps of
                   Nothing    -> S.fromList [opName]
                   Just deps' -> S.insert opName deps'
      curTainted = tainted state
      newTainted = if null $ S.intersection curTainted depSet
                   then curTainted else S.insert newAlias curTainted
  putState $ state { deps = M.insert newAlias depSet curDeps
                   , tainted = newTainted
                   }
addDeps _ _ = return ()

-- | Is the given operand tainted?
isTainted :: Operand -> Checker InputBasicState b Bool
isTainted (O.LocalReference _ opName) = do
  info <- getState
  return $ S.member opName $ tainted info
isTainted _                           = return False

-- | Taint the given name
taintName :: Name -> Checker InputBasicState b ()
taintName opName = do
  state <- getState
  newTainted <- getAliasesOf opName
  putState $ state { tainted = S.union newTainted $ tainted state}

-- | Taint the given operand
taint :: Operand -> Checker InputBasicState b ()
taint (O.LocalReference _ opName) = taintName opName
taint _                           = return ()

-- | Untaint the given operand
untaint :: Operand -> Checker InputBasicState b ()
untaint (O.LocalReference _ opName) = do
  info <- getState
  let curTainted = S.delete opName $ tainted info
  let curDeps = M.delete opName $ deps info
  -- Untainted any dependency of opName
      depOnName = S.delete opName $
                  S.fromList $ M.keys $ M.filter (S.member opName) curDeps
      finalDeps = foldr (\dep map' -> M.delete dep map') curDeps $ S.toList depOnName
      finalTainted = setDifference curTainted depOnName
  putState $ info { tainted = finalTainted
                  , deps = finalDeps
                  }
untaint _                           = return ()

--
-- Checker
--

-- | User input checker: look for cases where untrusted data flows to a sensitive operation
-- like a memcpy or a GEP index operation.
userInputCheck :: Int
               -> Named Instruction
               -> Checker InputBasicState InputBug ()
userInputCheck lineno ninstr = do
  filterFloats ninstr
  filterAsm ninstr
  case ninstr of

    -- Get any user input
    name := Call _ _ _ co argInfos _ _ -> do
      let funName = getCalledFunName co
      -- copyin(const void *uaddr, void *kaddr, size_t len);
      -- copyin_nofault(const void *uaddr, void *kaddr, size_t len);
      -- copyinstr(const void *uaddr, void *kaddr, size_t len, size_t *done);
      when ("copyin" `isInfixOf` show funName &&
           length argInfos >= 3) $ do
        let dangerArg = fst $ argInfos !! 1
        taint dangerArg

      when ( "Int32Value" `isInfixOf` show funName
           || "Uint32Value" `isInfixOf` show funName) $
        taintName name

    -- Is the user input used for a GEP or an allocation?
    Do (Call _ _ _ funName argInfos _ _) ->
      when (isMemCall funName) $ do
        let size = fst $ argInfos !! 2
        sizeIsTainted <- isTainted size
        -- Choose an arbitrary, large-seeming size to see if we can make the value
        -- what we want.
        when sizeIsTainted $ addInputBug size 4000 lineno
    name := GetElementPtr _ base is@(ind:inds) _ -> do
      if all isZero is
      then addAlias name base
      else when (isZero ind) $ addDeps name base

      -- check if the array is out of bounds
      indIsTainted <- isTainted ind
      when indIsTainted $ addInputBug ind 4000 lineno
      -- check if the inner types can be statically out of bounds
      let refTy = getRefTy' $ typeOf base
      types <- getTypes refTy inds []
      forM_ (zip inds types) $ \(ind', ty) -> do
        indexIsTainted <- isTainted ind'
        when indexIsTainted $ do
          numElems <- getNumElems ty
          when (isJust numElems) $ addInputBug ind' (fromJust numElems) lineno

    name := Load _ addr _ _ _ -> addDeps name addr

    -- Track the value once it arrives; for now, no memory
    -- We're looking for divide-by-zero and remainder by zero errors
    name := Add _ _ op1 op2 _ -> addDeps name op1 >> addDeps name op2
    name := Sub _ _ op1 op2 _ -> addDeps name op1 >> addDeps name op2
    name := Mul _ _ op1 op2 _ -> addDeps name op1 >> addDeps name op2
    name := UDiv _ op1 op2 _  -> addDeps name op1 >> addDeps name op2
    name := SDiv _ op1 op2 _  -> addDeps name op1 >> addDeps name op2
    name := URem op1 op2 _    -> addDeps name op1 >> addDeps name op2
    name := SRem op1 op2 _    -> addDeps name op1 >> addDeps name op2
    name := Shl _ _ op1 op2 _ -> addDeps name op1 >> addDeps name op2
    name := LShr _ op1 op2 _  -> addDeps name op1 >> addDeps name op2
    name := AShr _ op1 op2 _  -> addDeps name op1 >> addDeps name op2
    name := And op1 op2 _     -> addDeps name op1 >> addDeps name op2
    name := Or op1 op2 _      -> addDeps name op1 >> addDeps name op2
    name := Xor op1 op2 _     -> addDeps name op1 >> addDeps name op2
    name := Trunc op _ _      -> addDeps name op
    name := ZExt op _ _       -> addDeps name op
    name := SExt op _ _       -> addDeps name op
    name := BitCast op _ _    -> addAlias name op
    -- Untatint symbolic comparisons for now
    _ := ICmp _ op1 op2 _    ->
      when (isLocalReference op1 && isLocalReference op2) $ untaint op1 >> untaint op2
    _ := Select op1 op2 op3 _ ->
      when (isLocalReference op1 && isLocalReference op2) $
        untaint op1 >> untaint op2 >> untaint op3
    _ -> return ()

-- | Bug representation for this checker
data InputBug = InputBug { inputBugFilePath  :: FilePath
                         , inputBugFunction  :: Name
                         , inputBugPath      :: Path
                         , inputBugLine      :: Int
                         , inputBugSize      :: Int
                         , inputBugAffectVar :: Name
                         } deriving (Eq, Ord, Show)

-- | Add the bug to the set of all bugs that the checker has foud
addInputBug :: Operand -- ^ Tainted input
            -> Int -- ^ Target value for the input
            -> Int -- ^ Line on which potential bug appeared
            -> Checker InputBasicState InputBug ()
addInputBug op val lineno = do
  fp <- getFilepath
  fn <- getFunName
  path <- getPath
  addBug $ InputBug fp fn path lineno val $ nameOf' op
