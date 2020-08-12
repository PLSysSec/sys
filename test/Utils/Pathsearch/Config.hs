module Utils.Pathsearch.Config (
    PathSearchCfg(..)
  , pathCfgWithLength
  -- ** Path prefix configuration
  , module Utils.Pathsearch.PathPrefix
  -- ** Quit DSL
  , shouldQuit
  , dontQuit
  , module Utils.Pathsearch.BlockPredicate
  -- ** Call configurations
  , EnterCallCfg  -- opaque to other modules
  , dontEnterCalls
  , enterAllCalls
  , enterOnlyCalls
  , enterAllCallsBut
  , dynCallCfg
  , shouldHandleCall
  -- ** Targetted sliding window configuration
  , TargettedWindowCfg(..)
  , toTargettedWindowCfg
) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           LLVM.AST
import           InternalIR.SimplePath
import           Utils.Pathsearch.BlockPredicate
import           Utils.Pathsearch.PathPrefix

-- | Path search configurations.
data PathSearchCfg = PathSearchCfg {
    psHandleLoops  :: Bool
    -- ^ Handle loops
  , psEnterCalls   :: EnterCallCfg
  -- ^ Call enter configuration
  , psFollowRets   :: Bool
  -- ^ If true, follow returns. See $note1 at bottom of file.
  , psPathLength   :: Int
  -- ^ Number of basic blocks to explore
  , psQuitOn       :: BlockPredicate
  -- ^ We want to quit early on things we don't support (e.g., instructions
  -- with opaque or unsupported types)
  , psFilterPrefix :: Set PathPrefix
  -- ^ Optional: only consider paths which begin with one of the specified
  -- path prefixes. An empty set does not filter out any path.
  } deriving (Show)

-- | Helper for creating a path search config. Handles loops but does not cross calls or returns.
pathCfgWithLength :: Int -> PathSearchCfg
pathCfgWithLength l = PathSearchCfg { psHandleLoops  = True
                                    , psPathLength   = l
                                    , psEnterCalls   = dontEnterCalls
                                    , psFollowRets   = False
                                    , psQuitOn       = dontQuit
                                    , psFilterPrefix = Set.empty
                                    }

-- | How should we handle calls?
data EnterCallCfg = CallDenyList [Name]
                  -- ^ Enter all except named functions
                  | CallAllowList [Name]
                  -- ^ Enter only named functions
                  | CallDynamic (Name -> SimplePath -> Bool)
                  -- ^ Enter call depending on the current path

instance Eq EnterCallCfg where
  (CallDenyList ns0)  == (CallDenyList ns1) = ns0 == ns1
  (CallAllowList ns0) == (CallAllowList ns1) = ns0 == ns1
  _ == _ = False

instance Show EnterCallCfg where
  show (CallDenyList ns)  = "CallDenyList " ++ show ns
  show (CallAllowList ns) = "CallAllowList " ++ show ns
  show (CallDynamic _)    = "CallDynamic _"

-- | Don't enter any calls.
dontEnterCalls :: EnterCallCfg
dontEnterCalls = CallAllowList []

-- | Enter all possible calls.
enterAllCalls :: EnterCallCfg
enterAllCalls = CallDenyList []

enterOnlyCalls :: [Name] -> EnterCallCfg
enterOnlyCalls = CallAllowList

-- | Enter all possible calls except.
enterAllCallsBut :: [Name] -> EnterCallCfg
enterAllCallsBut = CallDenyList

-- | Create dynamic call configuration
dynCallCfg :: (Name -> SimplePath -> Bool) -> EnterCallCfg
dynCallCfg = CallDynamic

-- | Check if we need to handle call
shouldHandleCall :: EnterCallCfg -> Name -> SimplePath -> Bool
shouldHandleCall (CallAllowList ls) funcName _ = funcName `elem` ls
shouldHandleCall (CallDenyList ls) funcName _  = funcName `notElem` ls
shouldHandleCall (CallDynamic f) funcName path = f funcName path

--
-- Quit API
--

-- | Apply predicate to basic block.
shouldQuit :: BlockPredicate -> ModuleInfo -> BasicBlock -> Bool
shouldQuit = isTrue

-- | Don't quit predicate
dontQuit :: BlockPredicate
dontQuit = constPred False

--
-- Sliding window config
--

-- | A start target configuration narrows the paths we explore. We look for a
-- particular kind of block and start the path search a little bit before this
-- block. Note: when using a targetted configuration, we don't enter calls for
-- the "before" blocks. We only enter calls (if enter call is enabled)
data TargettedWindowCfg =  TargettedWindowCfg {
    swMustInclude :: BlockPredicate
  -- ^ Predicate specifying the kind of block the path must have
  , swGoBackBy    :: Int
  -- ^ How many blocks before this block should we start from?
  }

-- | Given an initial path search configuration and sliding window
-- configuration, produce a path search configuration. This function changes
-- the 'psEnterCalls' and 'psFilterPrefix'. In particular we (1) disable
-- call-entering for the first 'swGoBackBy' blocks and (2) filter
-- paths to satisfy the 'swMustInclude' predicate. NOTE: if a set of prefixes
-- is already defined, we don't perform step 2; we usually set prefixes for
-- debugging, so this should be fine.
toTargettedWindowCfg :: Name
                     -- ^ Function name
                     -> ModuleInfo
                     -- ^ LLVM module + precomputed module info
                     -> PathSearchCfg
                     -- ^ Original path search config
                     -> TargettedWindowCfg
                     -- ^ The sliding window config
                     -> PathSearchCfg
                     -- ^ Final path search config
toTargettedWindowCfg fName mInfo cfg swCfg =
  cfg { psEnterCalls   = dynCallCfg $ \n sp -> if length (bbs sp) <= swGoBackBy swCfg
                                                 then False
                                                 else shouldHandleCall (psEnterCalls cfg) n sp
      , psFilterPrefix = if Set.null exPrefixes
                           then swPrefixes
                           else exPrefixes
      }
  where -- Get all path prefixes given the sliding window configuration
        swPrefixes = getAllPathPrefixesWithSuffix fName mInfo (swGoBackBy swCfg + 1) (swMustInclude swCfg)
        -- Get existing prefixes
        exPrefixes = psFilterPrefix cfg

{- $note1

  This note explains why you generally want to follow returns.  Without
  following, pathsearch fails in some surprising/interesting ways.

  Consider, for instance, the C function:

    int simple(int x) {
      int y = x * 2;
      if (y > 25)
        return y;
      return simple(y);
    }

  which compiles to following the LLVM bitcode:

    define i32 @simple(i32) #0 {
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i32, align 4
      store i32 %0, i32* %3, align 4
      %5 = load i32, i32* %3, align 4
      %6 = add nsw i32 %5, 2
      store i32 %6, i32* %4, align 4
      %7 = load i32, i32* %4, align 4
      %8 = icmp sgt i32 %7, 25
      br i1 %8, label %9, label %11

    ; <label>:9:                        ; preds = %1
      %10 = load i32, i32* %4, align 4
      store i32 %10, i32* %2, align 4
      br label %14

    ; <label>:11:                       ; preds = %1
      %12 = load i32, i32* %4, align 4
      %13 = call i32 @simple(i32 %12)
      store i32 %13, i32* %2, align 4
      br label %14

    ; <label>:14:                       ; preds = %11, %9
      %15 = load i32, i32* %2, align 4
      ret i32 %15
    }

  Say we're looking for paths of length 3.  Without following returns, none of
  the paths of length 3 include the `store %13, %2`. Anywhere in the path.
  Also, none of the paths of length 3 start with BB 9 or BB 14.  They all start
  with the first BB or BB 11.
-}
