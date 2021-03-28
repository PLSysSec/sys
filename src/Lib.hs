{-|

This module runs each static and symbolic checker pair and outputs the results.
The more interesting code is in other files:
- Static/ contains all automatic static checking code
- Symex/ contains all automatic symbolic checking code

-}
module Lib ( -- * All final paper checkers, plus new checker heap oob
             uninit -- * For main
           , doUninitCheck -- * For testing
           , concrOOB -- * For main
           , doConcrOOBCheck -- * For testing
           , heapOOB -- * For main
           , doHeapOOBCheck -- * For testing
           , userInput -- * For main
           , doUserInputCheck -- * For testing
           , uaf -- * For main
             -- * Run your own static or symbolic checking
           , checkFiles
           , checkFile
           , symexPath
           )
where
import           System.Directory(renameFile)
import           Data.List.Split
import           Checkers.ConcreteOOBStatic
import           Checkers.HeapOOBStatic
import           Checkers.StaticConfigs.CheckerConfigs
import qualified Checkers.SymexConfigs.CheckerConfigs  as SymConfigs
import           Checkers.UAFStatic
import           Checkers.UninitStatic
import           Checkers.UserInputStatic
import           Control.Concurrent.MVar
import           Control.Concurrent.STM                (atomically)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Thread.Group
import           Control.Exception                     as E
import           Control.Log                           as L
import           Control.Monad
import           Data.ByteString                       (ByteString, readFile)
import qualified Data.Map                              as M
import           Data.Maybe                            (fromJust, isJust)
import qualified Data.Set                              as S
import           InternalIR.ModuleInfo
import           InternalIR.PathExpand
import           InternalIR.PathInfo
import           LLVM.AST
import           LLVM.Context
import           LLVMAST.Interface
import           Prelude                               hiding (log, mod, readFile)
import           Static.Check
import           Static.CheckerConfigDef
import qualified Symex.CheckerConfigDef                as SymConfig
import           Symex.Symex
import           System.IO.Unsafe                      (unsafePerformIO)
import           Utils

--
-- Uninit checker
--

-- | Run the uninitialized memory static and symbolic checker
uninit :: FilePath
       -> [String]
       -> IO ()
uninit fpath extns = do
  candidates <- checkFiles uninitConfig fpath extns
  candidateGroup <- new

  forM_ candidates $ \bug ->
    void $ forkIO candidateGroup $ E.handle printHandler $ do
      attackResult <- doUninitCheck bug
      showUninitResult bug attackResult
  wait candidateGroup

doUninitCheck :: UninitBug -> IO SolverResult
doUninitCheck (UninitBug idx path _ fname fp var ty) = do
  modInfo <- makeModuleInfo fp
  let staticPath = makeIntraproStaticPathStop modInfo fname path idx
  symexPath (SymConfigs.uninitSymexConfig var ty) [fname] [modInfo] staticPath

--
-- heap out of bounds checker
--

-- | Run the heap out of bounds static and symbolic checker
heapOOB :: FilePath
        -> [String]
        -> IO ()
heapOOB fpath extns = do
  candidates <- checkFiles heapoobConfig fpath extns
  candidateGroup <- new

  forM_ candidates $ \bug ->
   void $ forkIO candidateGroup $ E.handle printHandler $ do
      let word = "func"
      L.log L.INFO $ unwords $ ["Forking a thread for funciton Basic Block", word]
      result <- doHeapOOBCheck bug
      showMallocResult bug result
  wait candidateGroup

doHeapOOBCheck :: MOOBBug -> IO SolverResult
doHeapOOBCheck (MOOBBug (OOBInfo _ asize isize jsize ig _ _ _) fp fun p _) = do
  modInfo <- makeModuleInfo fp
  let staticPath = makeIntraproStaticPath modInfo fun p
  symexPath (SymConfigs.heapoobSymexConfig asize isize jsize ig) [fun] [modInfo] staticPath

--
-- User input taint checker
--

-- | Run the user input static and symbolic checker
userInput :: FilePath
          -> [String]
          -> IO ()
userInput fpath extns = do
  userInputIndecies <- checkFiles userInputConfig fpath extns
  candidateGroup <- new
  forM_ userInputIndecies $ \bug ->
    void $ forkIO candidateGroup $ E.handle printHandler $ do
    attackResult <- doUserInputCheck bug
    showUserInputResult bug attackResult
  wait candidateGroup

doUserInputCheck :: InputBug -> IO SolverResult
doUserInputCheck (InputBug fp fname path _ val var) = do
  modInfo <- makeModuleInfo fp
  let staticPath = makeIntraproStaticPath modInfo fname path
  symexPath (SymConfigs.userInputSymexConfig var val) [fname] [modInfo] staticPath

--
-- Concrete out of bounds checker
--

-- | Run the concrete out of bounds checker
concrOOB :: FilePath
         -> [String]
         -> IO ()
concrOOB fpath extns = do
  candidates <- checkFiles concroobConfig fpath extns
  candidateGroup <- new
  forM_ candidates $ \ bug ->
    void $ forkIO candidateGroup $ E.handle printHandler $ do
    attackResult <- doConcrOOBCheck bug
    showNegBug bug attackResult
  wait candidateGroup

doConcrOOBCheck :: COOBBug -> IO SolverResult
doConcrOOBCheck (COOBBug _ negIdx negAmt path fname fp) = do
  modInfo <- makeModuleInfo fp
  let staticPath = makeIntraproStaticPath modInfo fname path
  symexPath (SymConfigs.concroobSymexConfig negIdx negAmt) [fname] [modInfo] staticPath

--
-- UAF checker
--

-- | Run the UAF static and symbolic checker
uaf :: FilePath
    -> [String]
    -> IO ()
uaf fpath extns = do
  candidates <- checkFiles uafConfig fpath extns
  candidateGroup <- new

  forM_ candidates $ \bug ->
    void $ forkIO candidateGroup $ E.handle printHandler $ do
      -- L.log L.INFO $ "Symbolically checking static alert: " ++ show bug
      attackResult <- doUAFCheck bug
      showUAFResult bug attackResult
  wait candidateGroup

doUAFCheck :: UAFBug -> IO SolverResult
doUAFCheck (UAFBug fp fname path _ var ty _) = do
  modInfo <- makeModuleInfo fp
  let staticPath = makeIntraproStaticPath modInfo fname path
  symexPath (SymConfigs.uafSymexConfig var ty) [fname] [modInfo] staticPath

--
-- Making static paths
--

makeIntraproStaticPathStop :: ModuleInfo
                           -> Name
                           -> [Name]
                           -> Int
                           -> StaticPath
makeIntraproStaticPathStop modInfo funName path idx =
  makeSingleStaticPath modInfo funName path (Just idx)

makeIntraproStaticPath :: ModuleInfo
                       -> Name
                       -> [Name]
                       -> StaticPath
makeIntraproStaticPath modInfo funName path =
  makeSingleStaticPath modInfo funName path Nothing

makeSingleStaticPath :: ModuleInfo
                       -> Name
                       -> [Name]
                       -> Maybe Int
                       -> StaticPath
makeSingleStaticPath modInfo funName path mTerminalIdx = do
  case bbsFromFunctionName (modAST modInfo) funName of
    Nothing -> error $ "Could not find bbs for this path: " ++ show path
    Just bbs ->
      let bbMap = makeBBMap bbs
      in concatMap  (\(name, pathIndex) ->
                      -- Figure out the previous block
                     let prevBlock = if pathIndex == 0
                                     then Nothing
                                     else Just $ path !! (pathIndex - 1)
                         isLastBlock = pathIndex + 1 >= length path
                      -- Get the individual instruction entries
                         entry idx = StaticPathEntry modInfo funName name idx prevBlock
                         block = bbMap M.! name
                      -- Figure out which index to finish at:
                      -- If we're the last block and have a terminal index, finish there
                      -- Otherwise, go through each line of the block
                         maxEntry = if isLastBlock && isJust mTerminalIdx
                                    then (fromJust mTerminalIdx) - 1
                                    else (length $ getBlockContents block) -1
                         entries = map (\idxNum ->
                                         entry $ SPEInstr idxNum
                                        ) $ [0..maxEntry]
                      -- If there is a next block, add a terminator entry indicating it
                      in if isLastBlock
                         -- USED TO MAKE A TERMINAL FUNCTION CALL (?), NOW JUST TERMINATES
                         then if isJust mTerminalIdx
                              then let newEntry = SPEInstr (fromJust mTerminalIdx)
                                   in entries ++ [entry newEntry]
                              else entries
                         else let nextBlock = path !! (pathIndex + 1)
                              in entries ++ [entry $ SPETerm nextBlock]
              ) $ zip path [0..]


process :: String -> [String]
process file = do
  let a = [ b | b <- file]
  return a



--
--
-- Static/symbolic execution:
-- These three functions (checkFile, checkFile, and symexPath) allow
-- you to (1) statically check some files for suspicious elements and
-- then (2) symbolically execute any suspicious paths.
--
--
-- | Statically check all files in a directory
checkFiles :: (Show a, Show b, Ord b)
            => CheckerConfig a b
            -> FilePath
            -> [String]
            -> IO (S.Set b)
checkFiles ckConfig dirPath extns = do
  fileGroup  <- new
  resultQueue <- newTQueueIO
  sourceFiles <- getSourceFiles dirPath extns 
  --print sourceFiles
  let processedSF = map (process) sourceFiles
  let a = concat processedSF
  let b = chunksOf 2 a

  forM_ b $ \item -> do
    forM_ item $  \file -> void $ forkIO fileGroup $
                              checkFile resultQueue fileGroup ckConfig file
    forM_ item $ \file -> do 
      let filenew = file ++ ".done"
      renameFile file filenew

   
    wait fileGroup

  S.fromList <$> (atomically $ flushTQueue resultQueue)

-- | Statically check a single file
checkFile :: (Show a, Show b, Ord b)
          => TQueue b
          -> ThreadGroup
          -> CheckerConfig a b
          -> FilePath
          -> IO ()
checkFile resultQueue fileGroup ckConfig path = do
  L.log L.INFO $ unwords $ ["Checking file...", path]
  contents <- readFile path
  when (cfgShouldCheckFile ckConfig contents) $ do
    withContext $ \ctx -> E.handle printHandler $ do
      modInfo <- getModInfoCache path ctx contents
      let mod =  modAST modInfo
      when (cfgShouldCheckModule ckConfig $ modAST modInfo) $ do
        L.log L.DEBUG $ unwords $ ["Checking module...", show $ moduleName mod]
        forM_ (getFunctionDefns mod) $ \defn -> do
          when (cfgShouldCheckFunction ckConfig defn) $ void $ forkIO fileGroup $ do
            L.log L.DEBUG $ unwords $ ["Checking function...", show $ getFunctionName' defn]
            staticCheckFunctionAsync resultQueue fileGroup path modInfo defn ckConfig
    where getFunctionDefns :: Module -> [Definition]
          getFunctionDefns = filter isFunction . moduleDefinitions

symexPath :: SymConfig.Config a
          -> [Name] -- ^ Names, with the start function first
          -> [ModuleInfo] -- ^ Module infos, with the start module info first
          -> StaticPath
          -> IO SolverResult
symexPath config names modInfos staticPath = do
  let path = staticToSimple staticPath
      pl = SymConfig.pathLength config
      ab = SymConfig.arrayBound config
      pathInfos = map (\(mi, name) -> getPathInfo mi pl name path ab) $ zip modInfos names
      pathInfo = foldr mergePathInfos (head pathInfos) (tail pathInfos)
  symex config path pathInfo

moduleInfoMap :: MVar (M.Map FilePath ModuleInfo)
{-# NOINLINE moduleInfoMap #-}
moduleInfoMap = unsafePerformIO $ newMVar M.empty

getModInfoCache :: FilePath -> Context -> ByteString -> IO ModuleInfo
getModInfoCache path ctx contents = do
  map0 <- readMVar moduleInfoMap
  case M.lookup path map0 of
    Just mInfo -> return mInfo
    Nothing -> do mInfo <- getModuleInfo ctx contents
                  modifyMVar_ moduleInfoMap (\map1 -> return $ M.insert path mInfo map1)
                  return mInfo

makeModuleInfo :: FilePath -> IO ModuleInfo
makeModuleInfo path = do
  map0 <- readMVar moduleInfoMap
  case M.lookup path map0 of
    Just mInfo -> return mInfo
    Nothing -> do mInfo <- withContext $ \ctx -> E.handle handler $ getModuleInfoFromFile ctx path
                  modifyMVar_ moduleInfoMap (\map1 -> return $ M.insert path mInfo map1)
                  return mInfo
 where
   handler :: E.SomeException -> IO ModuleInfo
   handler e = error $ show e

printHandler :: E.SomeException -> IO ()
printHandler = print
