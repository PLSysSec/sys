{-|

This module is the simple UAF checker. Its symbolic counterpart is in
UAFSymbolic.hs.

-}
module Checkers.UAFStatic ( uafCheck
                          , blankUAFState
                          , UAFState
                          , UAFBug(..)
                          ) where
import           Checkers.Utils.StaticUtils
import           Control.Monad              (forM_, unless, when)
import qualified Checkers.UAFSymbolic       as U
import           Data.List                  (isInfixOf)
import qualified Data.Set                   as S
import           LLVM.AST                   hiding (args)
import qualified LLVM.AST.Operand           as O
import qualified LLVM.AST.Constant           as C
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getFunName)
import           Static.CheckerState

-- | Identify suspicious-looking code snippets that might be uses of feed
-- memory.  The checker matches on each LLVM instruction ('ninstr') to update
-- the internal state.
uafCheck :: Int -> Named Instruction -> UAFChecker ()
uafCheck lineno ninstr = do
  -- filterAsm ninstr
  filterFloats ninstr
  filterAsserts ninstr
  modelAssigns ninstr

  case ninstr of

    -- If the cast thing or the GEP base is freed, mark the rhs free
    name := GetElementPtr _ addr _ _ -> addAlias name addr

    _ := BitCast op _ _ | isTreeNode $ typeOf op -> die

    -- Mark the freed argument as free
    Do (Call _ _ _ (Right func) ops _ _) | U.isFree func ->
      forM_ (map fst ops) $ \op ->
        case nameOf op of
          Nothing -> return ()
          Just varName -> do
            free <- isFree op
            if free
              then addUAFBug op lineno -- double free
              else dieIfRefCountingCode >> freeVar varName func

    -- If it's a call, treat all freed pointer arguments as a bug
    _ | (Call _ _ _ (Right func) args _ _) <- getInstruction ninstr -> do
      unless (U.isLLVMLifetimeFunc func) $ forM_ args $ \argInfo -> do
        let arg = fst argInfo
        free <- isFree arg
        when free $ addUAFBug arg lineno

    -- Flag store to freed memory
    Do (Store _ addr _ _ _ _)  -> do
      free <- isFree addr
      when free $ addUAFBug addr lineno

    -- Flag load from freed memory
    _ := Load _ addr _ _ _ -> do
      free <- isFree addr
      when free $ addUAFBug addr lineno

    _ -> return ()

-- | Free variable
freeVar :: Name -> Operand -> UAFChecker ()
freeVar name freeOp = do
  s0 <- getState
  putState $ s0 { freed = S.insert name (freed s0)
                , freeName = nameOf freeOp
                }

-- | True if any aliases are free
isFree :: Operand -> UAFChecker Bool
isFree (O.LocalReference _ name) = do
  aliases <- getAliasesOf name
  s0 <- freed <$> getState
  return $ not $ null $ S.intersection s0 aliases
isFree _ = return False

isTreeNode :: Type -> Bool
isTreeNode ty | "class.std::__1::__tree_end_node" `isInfixOf` show ty = True
              | otherwise = False


data UAFState = UAFState { freed    :: S.Set Name      -- ^ Freed
                         , reported :: S.Set Name      -- ^ Used and reported
                         , freeName :: Maybe Name
                         } deriving (Eq, Ord, Show)


blankUAFState :: UAFState
blankUAFState = UAFState S.empty S.empty Nothing

type UAFChecker a = Checker UAFState UAFBug a

data UAFBug = UAFBug { uafBugFilePath :: FilePath
                     , uafBugFunction :: Name
                     , uafBugPath     :: Path
                     , uafBugLine     :: Int
                     , uafBugFreedVar :: Name
                     , uafBugType     :: Type
                     , uafFreeName    :: Maybe Name
                     } deriving (Eq, Ord, Show)


-- | Add the bug to the set of all bugs that the checker has foud
addUAFBug :: Operand -- ^ Freed operand
          -> Int -- ^ Line on which potential bug appeared
          -> UAFChecker ()
addUAFBug op lineno = do
  aliases <- getAliasesOf name

  -- Get the the reported variables and their sizes
  s0 <- getState
  let reps  = reported s0
      fname = freeName s0

  when (null $ S.intersection reps aliases) $ do
    reportVar name
    fp <- getFilepath
    fn <- getFunName
    path <- getPath
    addBug $ UAFBug { uafBugFilePath  = fp
                    , uafBugFunction  = fn
                    , uafBugPath      = path
                    , uafBugLine      = lineno
                    , uafBugFreedVar  = name
                    , uafBugType      = ty
                    , uafFreeName     = fname
                    }
  where name = nameOf' op
        ty   = typeOf op

-- | Report variable
reportVar :: Name -> UAFChecker ()
reportVar name = do
  s0 <- getState
  putState $ s0 { reported = S.insert name (reported s0) }

-- | Supress reference counting code. This is a very rough supression
-- mechanism. Should probably only look at 1-3 BBs back not all.
dieIfRefCountingCode :: UAFChecker()
dieIfRefCountingCode = do
  bbs <- getBlockList
  when (any domatch $ reverse $ concat $ map unBB bbs) $ die
  where unBB (BasicBlock _ ni _) = ni
        domatch (_ := AtomicRMW {}) = True
        -- check if adding -1
        domatch (_ := (Add _ _ _ (ConstantOperand (C.Int _ n1)) _)) = n1 == 18446744073709551615
        domatch _ = False
