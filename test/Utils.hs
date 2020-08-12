module Utils ( allSat
             , anySat
             , allUnsat
             , anyUnsat
             , allError
             , anyError
             , noSat
             , testConfig
             , failUnimplemented
             , numberSatTest
             , basicDontAnalyzeTest
             , basicAllSatTest
             , allSatTest
             , basicAnySatTest
             , anySatTest
             , basicNoSatTest
             , basicAllUnsatTest
             , basicAnyUnsatTest
             , basicNoUnsatTest
             , basicAllErrorTest
             , basicAnyErrorTest
             , variableAssignmentTestAlias
             , variableAssignmentTest
             , vtest
             , boundedVariableAssignmentTest
             , boundedVariableAssignmentTestCalls
             , boundedVariableAssignmentTestCallsAndReturns
             , withModule
             , withModuleAndContext
             , module BenchUtils
             ) where
import           BenchUtils
import           Checkers.SymexConfigs.CheckerConfigs (testConfig)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Thread.Group
import           Control.Monad                        (forM, forM_, unless, void, when)
import           Control.Monad.STM                    (atomically)
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import           InternalIR.ModuleInfo
import           Lib                                  (checkFiles)
import           LLVM.Context
import           Static.CheckerConfigDef
import           Symex.CheckerConfigDef
import           Symex.Symex                          (SolverResult (..), solverFailed, solverSat,
                                                       solverUnsat)
import           Test.Tasty.HUnit
import           Utils.Pathsearch

allSat :: [SolverResult] -> Bool
allSat = all solverSat

anySat :: [SolverResult] -> Bool
anySat = any solverSat

numSat :: [SolverResult] -> Int
numSat = length . filter solverSat

allUnsat :: [SolverResult] -> Bool
allUnsat = all solverUnsat

anyUnsat :: [SolverResult] -> Bool
anyUnsat = any solverUnsat

allError :: String -> [SolverResult] -> Bool
allError e = all $ errorIs e

anyError :: String -> [SolverResult] -> Bool
anyError e = any $ errorIs e

-- Different from 'allUnsat', because it also accepts Faileds
noSat :: [SolverResult] -> Bool
noSat = not . any solverSat

errorIs :: String -> SolverResult -> Bool
errorIs e1 (SolverFailed e2) = e1 == e2
errorIs _ _                  = False

firstFail :: [SolverResult] -> Maybe SolverResult
firstFail res = case filter solverFailed res of
                  failed:_ -> Just failed
                  []       -> Nothing

failUnimplemented :: String -> BenchTest
failUnimplemented testName = benchTestCase testName $ assertFailure "Unimplemented"

basicDontAnalyzeTest :: Config a -> String -> BenchTest
basicDontAnalyzeTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should not analyze the file" $ null res

-- Some number of results SAT, static and symbolic
numberSatTest :: (Show a, Show b, Ord b)
              => CheckerConfig a b
              -> (b -> IO SolverResult)
              -> Int
              -> String
              -> [String]
              -> BenchTest
numberSatTest staticConfig doCheck expectedNum path extns = benchTestCase path $ do
  candidates <- checkFiles staticConfig path extns
  candidateGroup <- new
  resultQueue <- newTQueueIO
  forM_ (S.toList candidates) $ \bug ->
    void $ forkIO candidateGroup $ do
      result <- doCheck bug
      atomically $ writeTQueue resultQueue result
  wait candidateGroup
  results <- atomically $ flushTQueue resultQueue
  assertBool "Should analyze the file" $ not $ null results
  let numBugs = numSat results
  assertBool (unwords ["Expect at least", show expectedNum, "sat"]) $ numBugs >= expectedNum
--  expectedNum @=? numSat results

-- All SAT, no UNSAT or FAILED
basicAllSatTest :: Config a -> String -> BenchTest
basicAllSatTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  case firstFail res of
    Just f  -> assertFailure $ show f
    Nothing -> assertBool "File should produce all SAT results" $ allSat res

-- All results are SAT, uses both static and symbolic
allSatTest :: (Show a, Show b, Ord b)
           => CheckerConfig a b
           -> (b -> IO SolverResult)
           -> String
           -> [String]
           -> BenchTest
allSatTest staticConfig doCheck path extns = benchTestCase path $ do
  candidates <- checkFiles staticConfig path extns
  results <- forM (S.toList candidates) doCheck
  assertBool "Should analyze the file" $ not $ null results
  assertBool "File should produce all SAT results" $ allSat $ results

-- At least one SAT.  The others can be SAT, UNSAT, or FAILED and the test still passes
basicAnySatTest :: Config a -> String -> BenchTest
basicAnySatTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  assertBool "File should produce a SAT result" $ anySat res

-- At least one SAT, static and symbolic checker
anySatTest :: (Show a, Show b, Ord b)
           => CheckerConfig a b
           -> (b -> IO SolverResult)
           -> String
           -> [String]
           -> BenchTest
anySatTest staticConfig doCheck path extns = benchTestCase path $ do
  candidates <- checkFiles staticConfig path extns
  results <- forM (S.toList candidates) doCheck
  assertBool "Should analyze the file" $ not $ null results
  assertBool "File should produce all SAT results" $ anySat $ results

-- No SATs. Either Unsat or FAILED
basicNoSatTest :: Config a -> String -> BenchTest
basicNoSatTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "File should not produce SAT result" $ noSat res

-- All UNSAT, no SAT or FAILED
basicAllUnsatTest :: Config a -> String -> BenchTest
basicAllUnsatTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  case firstFail res of
    Just f  -> assertFailure $ show f
    Nothing -> assertBool "File should produce all UNSAT results" $ allUnsat res

-- At least one UNSAT.  The others can be SAT, UNSAT, or FAILED and the test still passes
basicAnyUnsatTest :: Config a -> String -> BenchTest
basicAnyUnsatTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  assertBool "File should produce an UNSAT result" $ anyUnsat res

-- No UNSATs. They can be SAT or FAILED
basicNoUnsatTest :: Config a -> String -> BenchTest
basicNoUnsatTest cfg testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  assertBool "File should produce no UNSAT results" $ not $ anyUnsat res

-- All results should be FAILED with the given error message
basicAllErrorTest :: Config a -> String -> String -> BenchTest
basicAllErrorTest cfg e testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  let errorMsg = unwords ["File should produce", e]
  assertBool errorMsg $ allError e res

-- At least one FAILED with the given error message; the others can be SAT, UNSAT, or FAILED
basicAnyErrorTest :: Config a -> String -> String -> BenchTest
basicAnyErrorTest cfg e testPath = benchTestCase testPath $ do
  res <- symexFile cfg testPath []
  assertBool "Should analyze the file" $ not $ null res
  let errorMsg = unwords ["File should produce", e]
  assertBool errorMsg $ anyError e res

variableAssignmentTestAlias :: Map String Integer
                            -> String
                            -> BenchTest
variableAssignmentTestAlias = vtest testConfig

variableAssignmentTest :: Map String Integer
                       -> String
                       -> BenchTest
variableAssignmentTest = vtest testConfig

boundedVariableAssignmentTest :: Int
                              -> Map String Integer
                              -> String
                              -> BenchTest
boundedVariableAssignmentTest bound = vtest (testConfig { pathLength = bound })

boundedVariableAssignmentTestCalls :: Int
                                   -> Map String Integer
                                   -> String
                                   -> BenchTest
boundedVariableAssignmentTestCalls bound = vtest (testConfig { pathLength = bound
                                                             , callsOn = True
                                                             , verbose = True
                                                             })

boundedVariableAssignmentTestCallsAndReturns :: Int
                                             -> Map String Integer
                                             -> String
                                             -> BenchTest
boundedVariableAssignmentTestCallsAndReturns bound = vtest (testConfig { pathLength = bound
                                                                       , callsOn = True
                                                                       , returnFollowOn = True
                                                                       , verbose = True
                                                                       })

-- | Passes if the map is contained in any results
vtest :: Config a
      -> Map String Integer
      -> String
      -> BenchTest
vtest config expectedVars testPath = benchTestCase testPath $ do
  symexResult <- symexFile config testPath []
  when (null symexResult) $ error $ unwords $ [ "Expected SAT but got"
                                              , show symexResult ]
  unless (any checkResult symexResult) $
    error $ unwords [ "Expected to find"
                    , show expectedVars
                    , "in"
                    , unlines $ map show' symexResult ]
        -- check if expectedVars appear in result
  where checkResult (SolverSat res) = all (isInMap res) eVars
        checkResult _               = False
        -- check if (name, expect value) in the result map
        isInMap m (name, expectedValue) = case M.lookup name m of
          Just actualValue -> actualValue == expectedValue
          _                -> False
        -- expected values as list
        eVars = M.toList expectedVars
        -- prettier show for results
        show' (SolverSat res)  = unlines $ "SAT Model:" : (map show $ M.toList res)
        show' SolverUnsat      = "UNSAT"
        show' (SolverFailed e) = "FAILED: " ++ e

-- | Acquire an LLVM Module, rename its variables properly, and precompute
-- ModuleInfo, then execute the given action
withModule :: FilePath -> (ModuleInfo -> IO a) -> IO a
withModule path act = withContext $ \ctx -> getModuleInfoFromFile ctx path >>= act

withModuleAndContext :: FilePath -> (Context -> ModuleInfo -> IO a) -> IO a
withModuleAndContext path act = withContext $ \ctx -> getModuleInfoFromFile ctx path >>= (act ctx)
