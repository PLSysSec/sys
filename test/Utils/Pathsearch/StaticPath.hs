module Utils.Pathsearch.StaticPath (
    StaticPathEntry(..)
  , StaticPathEntryTerminator(..)
  , StaticPath
  , staticToSimple
) where

import           Data.List             (nub)
import qualified Data.Set              as Set
import           LLVM.AST
import           LLVMAST.ASTInterface
import           InternalIR.SimplePath
import           Utils.Pathsearch.Config
import           Utils.Pathsearch.Pathsearch

data StaticPathEntry = StaticPathEntry {
    speModule     :: ModuleInfo
  , speFunc       :: Name  -- ^ should be a function in the speModule
  , speBasicBlock :: BasicBlock  -- ^ should be a block in the speFunc
  , speTerminator :: StaticPathEntryTerminator  -- ^ how are we leaving this BasicBlock
}
  deriving (Eq)

instance Show StaticPathEntry where
  show spe = "<module " ++ modName (speModule spe) ++
             ", function " ++ show (speFunc spe) ++
             ", bb " ++ show (getBlockName $ speBasicBlock spe) ++
             ", term " ++ show (speTerminator spe) ++
             ">"

data StaticPathEntryTerminator = NotViaCall   -- ^ we leave this BasicBlock via anything other than a call, e.g. branch,
                                              -- condbranch, ret. I.e., we leave via the actual block terminator.
                               | ViaCall Name -- ^ we leave this BasicBlock via a call to the specified function
  deriving (Eq, Show)

type StaticPath = [StaticPathEntry]

-- | Returns either a SimplePath for the corresponding StaticPath, or a String error message
staticToSimple :: StaticPath -> IO (Either String SimplePath)
staticToSimple []       = return $ Right emptySimplePath  -- this way we can use `head statpath` safely in the main definition
staticToSimple statpath = do
  let mods = nub $ map speModule statpath
  sps <- getAllSimplePaths cfg (speFunc $ head statpath) (speModule $ head statpath) mods
  return $ case sps of
    [sp] -> Right sp
    []   -> Left $ "staticToSimple: couldn't find simple path for a static path:\n" ++ fancyShowStaticPath statpath
    _    -> Left $ "staticToSimple: found multiple simple paths for a single static path:\n" ++ fancyShowStaticPath statpath
  where cfg = PathSearchCfg {
      psHandleLoops = True
    , psEnterCalls = dynCallCfg $ enterIfStaticSaysSo statpath
    , psFollowRets = True
    , psPathLength = length statpath
    , psQuitOn = dontQuit
    , psFilterPrefix = Set.singleton $ staticToFilterPrefix statpath
  }

staticToFilterPrefix :: StaticPath -> PathPrefix
staticToFilterPrefix = map (\spe -> (modName (speModule spe), speFunc spe, getBlockName (speBasicBlock spe)))

enterIfStaticSaysSo :: StaticPath -- ^ Static path we're following
                    -> Name       -- ^ Function name we're asking about entering
                    -> SimplePath -- ^ Path we've traversed so far
                    -> Bool       -- ^ Enter that function?
{- (old implementation based on looking at next block in static path)
enterIfStaticSaysSo statpath _ simppath | length statpath <= length (bbs simppath) = False  -- we're done after this bb
enterIfStaticSaysSo statpath funcName simppath =
  let lenSoFar = length $ bbs simppath  -- if we were to enter funcName, that would be the (lenSoFar+1)'th bb
      intendedFunc = speFunc $ statpath !! lenSoFar  -- the function containing the (lenSoFar+1)'th bb in the statpath
  in  funcName == intendedFunc  -- enter if and only if we want to be inside that function for our next bb
-}
enterIfStaticSaysSo statpath funcName simppath =
  let lenSoFar = length $ bbs simppath
      currentSPE = statpath !! (lenSoFar - 1)  -- avoid off-by-one error
  in  speTerminator currentSPE == ViaCall funcName

fancyShowStaticPath :: StaticPath -> String
fancyShowStaticPath []       = "<empty path>"
fancyShowStaticPath statpath = fancyShowStaticPath' Nothing statpath

fancyShowStaticPath' :: Maybe StaticPathEntry -- ^ previous SPE printed
                     -> StaticPath            -- ^ path left to print
                     -> String
fancyShowStaticPath' _ [] = ""
fancyShowStaticPath' mspe (curSPE:statpath) =
  fancyShowNextPathEntry mspe curSPE ++ fancyShowStaticPath' (Just curSPE) statpath
  where fancyShowNextPathEntry :: Maybe StaticPathEntry -> StaticPathEntry -> String
        fancyShowNextPathEntry Nothing spe = showAll spe
        fancyShowNextPathEntry (Just prevSPE) spe | modName (speModule prevSPE) /= modName (speModule spe) = showAll spe
        fancyShowNextPathEntry (Just prevSPE) spe | speFunc prevSPE /= speFunc spe = showFuncOnward spe
        fancyShowNextPathEntry _              spe = showBBOnward spe
        showAll spe = "Module " ++ modName (speModule spe) ++ "\n" ++ showFuncOnward spe
        showFuncOnward spe = "  Function " ++ show (speFunc spe) ++ "\n" ++ showBBOnward spe
        showBBOnward spe = "    BB " ++ show (getBlockName $ speBasicBlock spe) ++
                           ", " ++ show (speTerminator spe) ++ "\n"
