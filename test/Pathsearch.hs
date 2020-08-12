{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Pathsearch ( pathSearchTests ) where

import           Data.ByteString.UTF8 (toString)
import           Data.ByteString.Short (fromShort)
import           Data.List (sort, sortOn, nub, intercalate)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           LLVM.AST hiding (nsw, nuw)
import           LLVM.Prelude hiding (void)
import           LLVMAST.ASTInterface
import           InternalIR.SimplePath
import           Utils.Pathsearch.Config
import           Utils.Pathsearch.Pathsearch

import           Control.Exception
import           System.Directory
import           System.Directory.Internal.Prelude (isDoesNotExistError)
import           System.Environment (lookupEnv)
import           Test.Tasty.HUnit

import           Utils

default (Integer, ShortByteString)

llvmPath :: String
llvmPath = "test/Pathsearch/llvm/"

pathSearchTests :: BenchTest
pathSearchTests = benchTestGroup "Pathsearch" [ simple1_tests
                                              , simple2_tests
                                              , simple2_call_tests
                                              , simple2_call2_tests
                                              , simple3_tests
                                              , endToEnd3_tests
                                              , simple_phi_tests
                                              , loop_tests
                                              , multipleCallers_tests
                                              , recursive_tests
                                              , switch_tests
                                              , filterPrefix_tests
                                              , crossModule_tests
                                              ]


simple1_tests = benchTestGroup "simple1.c" [
    benchTestGroup "getAllBasicBlockPathsOfLength" [
        bbTest "simple1" 1 [ [3], [12], [13], [16], [19], [25] ]
      , bbTest "simple1" 2 [ [3, 12], [3, 13]
                           , [12, 25]
                           , [13, 16], [13, 19]
                           , [16, 25]
                           , [19, 25] ]
      , bbTest "simple1" 3 [ [3, 12, 25], [3, 13, 16], [3, 13, 19]
                           , [13, 16, 25], [13, 19, 25] ]
      , bbTest "simple1" 4 [ [3, 13, 16, 25], [3, 13, 19, 25] ]
    --   -------------------------------------------------------------------------------
      , bbTest "simple1-O2" 1 [ [3], [5], [7], [9], [12] ]
      , bbTest "simple1-O2" 2 [ [3, 5], [3, 12]
                              , [5, 7], [5, 9]
                              , [7, 12]
                              , [9, 12] ]
      , bbTest "simple1-O2" 3 [ [3, 5, 7], [3, 5, 9]
                              , [5, 7, 12], [5, 9, 12] ]
      , bbTest "simple1-O2" 4 [ [3, 5, 7, 12], [3, 5, 9, 12] ]
      ]
  , benchTestGroup "getAllSimplePathsOfLength" $ map doTestN [1..4]
  ]
  where bbTest file = bbPathTest file "foo"
        doTestN = simplePathTestN False "simple1-O2" "foo"

simple2_tests = benchTestGroup "simple2.c" [
    benchTestGroup "getAllBasicBlockPathsOfLength" [
      benchTestGroup "no loop" [
          bbTest 1 [ [3], [8], [12], [17], [20] ]
        , bbTest 2 [ [3, 8]
                   , [8, 12], [8, 20]
                   , [12, 17]
                   , [17, 8] ]
        , bbTest 3 [ [3, 8, 12],  [3, 8, 20]
                   , [8, 12, 17]
                   , [12, 17, 8]
                   , [17, 8, 12], [17, 8, 20] ]
        , bbTest 4 [ [3, 8, 12, 17]
                   , [12, 17, 8, 20] ]
        , bbTest 5 [ ] -- no loops, no paths!
        , bbTest 6 [ ] -- no loops, no paths!
        , bbTest 7 [ ] -- no loops, no paths!
      ],
      benchTestGroup "loop" [
          bbTestL 1 [ [3], [8], [12], [17], [20] ]
        , bbTestL 2 [ [3, 8]
                    , [8, 12], [8, 20]
                    , [12, 17]
                    , [17, 8] ]
        , bbTestL 3 [ [3, 8, 12],  [3, 8, 20]
                    , [8, 12, 17]
                    , [12, 17, 8]
                    , [17, 8, 12], [17, 8, 20] ]
        , bbTestL 4 [ [3, 8, 12, 17]
                    , [8, 12, 17, 8]
                    , [12, 17, 8, 12], [12, 17, 8, 20]
                    , [17, 8, 12, 17] ]
        , bbTestL 5 [ [3, 8, 12, 17, 8]
                    , [8, 12, 17, 8, 12], [8, 12, 17, 8, 20]
                    , [12, 17, 8, 12, 17]
                    , [17, 8, 12, 17, 8] ]
        , bbTestL 6 [ [3, 8, 12, 17, 8, 12], [3, 8, 12, 17, 8, 20]
                    , [8, 12, 17, 8, 12, 17]
                    , [12, 17, 8, 12, 17, 8]
                    , [17, 8, 12, 17, 8, 12], [17, 8, 12, 17, 8, 20] ]
        , bbTestL 7 [ [3, 8, 12, 17, 8, 12, 17]
                    , [8, 12, 17, 8, 12, 17, 8]
                    , [12, 17, 8, 12, 17, 8, 12], [12, 17, 8, 12, 17, 8, 20]
                    , [17, 8, 12, 17, 8, 12, 17] ]
      ]
    ],
    benchTestGroup "getAllSimplePathsOfLength" [
      benchTestGroup "no loop" $ map doTestN [2..7]
    , benchTestGroup "loop" $ map doTestL [2..7]
    ]
  ]
  where bbTest = bbPathTest "simple2" "foo"
        bbTestL = bbPathTestL "simple2" "foo"
        doTestN = simplePathTestN False "simple2" "foo"
        doTestL = simplePathTestL False "simple2" "foo"

simple3_tests = benchTestGroup "simple3.c" [
    benchTestGroup "getAllBasicBlockPathsOfLength" [
      benchTestGroup "no loop" [
          bbTest 1 [ [0], [4], [7], [12], [15] ]
        , bbTest 2 [ [0, 4]
                   , [4, 7], [4, 15]
                   , [7, 12]
                   , [12, 4] ]
        , bbTest 3 [ [0, 4, 7], [0, 4, 15]
                   , [4, 7, 12]
                   , [7, 12, 4]
                   , [12, 4, 7], [12, 4, 15] ]
        , bbTest 4 [ [0, 4, 7, 12]
                   , [7, 12, 4, 15] ]
      ],
      benchTestGroup "loop" [
          bbTestL 1 [ [0], [4], [7], [12], [15] ]
        , bbTestL 2 [ [0, 4]
                    , [4, 7], [4, 15]
                    , [7, 12]
                    , [12, 4] ]
        , bbTestL 3 [ [0, 4, 7], [0, 4, 15]
                    , [4, 7, 12]
                    , [7, 12, 4]
                    , [12, 4, 7], [12, 4, 15] ]
        , bbTestL 4 [ [0, 4, 7, 12]
                    , [4, 7, 12, 4]
                    , [7, 12, 4, 7], [7, 12, 4, 15]
                    , [12, 4, 7, 12] ]
        , bbTestL 5 [ [0, 4, 7, 12, 4]
                    , [4, 7, 12, 4, 7], [4, 7, 12, 4, 15]
                    , [7, 12, 4, 7, 12]
                    , [12, 4, 7, 12, 4] ]
        , bbTestL 6 [ [0, 4, 7, 12, 4, 7], [0, 4, 7, 12, 4, 15]
                    , [4, 7, 12, 4, 7, 12]
                    , [7, 12, 4, 7, 12, 4]
                    , [12, 4, 7, 12, 4, 7], [12, 4, 7, 12, 4, 15] ]
        , bbTestL 7 [ [0, 4, 7, 12, 4, 7, 12]
                    , [4, 7, 12, 4, 7, 12, 4]
                    , [7, 12, 4, 7, 12, 4, 7], [7, 12, 4, 7, 12, 4, 15]
                    , [12, 4, 7, 12, 4, 7, 12] ]
        , bbTestL 8 [ [0, 4, 7, 12, 4, 7, 12, 4]
                    , [4, 7, 12, 4, 7, 12, 4, 7], [4, 7, 12, 4, 7, 12, 4, 15]
                    , [7, 12, 4, 7, 12, 4, 7, 12]
                    , [12, 4, 7, 12, 4, 7, 12, 4] ]
        , bbTestL 9 [ [0, 4, 7, 12, 4, 7, 12, 4, 7], [0, 4, 7, 12, 4, 7, 12, 4, 15]
                    , [4, 7, 12, 4, 7, 12, 4, 7, 12]
                    , [7, 12, 4, 7, 12, 4, 7, 12, 4]
                    , [12, 4, 7, 12, 4, 7, 12, 4, 7], [12, 4, 7, 12, 4, 7, 12, 4, 15] ]
        , bbTestL 10 [ [0, 4, 7, 12, 4, 7, 12, 4, 7, 12]
                     , [4, 7, 12, 4, 7, 12, 4, 7, 12, 4]
                     , [7, 12, 4, 7, 12, 4, 7, 12, 4, 7], [7, 12, 4, 7, 12, 4, 7, 12, 4, 15]
                     , [12, 4, 7, 12, 4, 7, 12, 4, 7, 12] ]
        , bbTestL 11 [ [0, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4]
                     , [4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 7], [4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 15]
                     , [7, 12, 4, 7, 12, 4, 7, 12, 4, 7, 12]
                     , [12, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4] ]
        , bbTestL 12 [ [0, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 7], [0, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 15]
                     , [4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 7, 12]
                     , [7, 12, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4]
                     , [12, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 7], [12, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 15] ]
      ]
    ]
  ]
    where bbTest = bbPathTest "simple3" "foo"
          bbTestL = bbPathTestL "simple3" "foo"


endToEnd3_tests = benchTestGroup "simple3.c" [ benchTestGroup "loop = 3" [ endToEnd3_3 ] ]

endToEnd3_3 :: BenchTest
endToEnd3_3 =
  -- the long path:
  -- [0, 4, 7, 12, 4, 7, 12, 4, 7, 12, 4, 15]
  -- tracking %5
  --  0  0  0   1  1  1   2  2  2   3  3   3
  let expectedVars = M.fromList [ ("foo_5", 0)
                                -- , ("foo_6", 1)

                                , ("foo_8", 44)
                                , ("foo_9", 132)
                                , ("foo_10", 0)
                                , ("foo_11", 132)

                                , ("foo_13", 0)
                                , ("foo_14", 1)

                                , ("foo_5_1", 1)
                                -- , ("foo_6_1", 1)

                                , ("foo_8_1", 44)
                                , ("foo_9_1", 132)
                                , ("foo_10_1", 132)
                                , ("foo_11_1", 264)

                                , ("foo_13_1", 1)
                                , ("foo_14_1", 2)

                                , ("foo_5_2", 2)
                                -- , ("foo_6_7", 1)

                                , ("foo_8_2", 44)
                                , ("foo_9_2", 132)
                                , ("foo_10_2", 264)
                                , ("foo_11_2", 396)

                                , ("foo_13_2", 2)
                                , ("foo_14_2", 3)

                                , ("foo_5_3", 3)
                                -- , ("foo_6_10", 0)

                                , ("foo_16", 396)
                                ]
  in boundedVariableAssignmentTest 12 expectedVars $ llvmPath ++ "simple3.ll"

simple2_call_tests = benchTestGroup "simple2_call.c" [
    benchTestGroup "getAllSimplePathsOfLength" [
      benchTestGroup "no loop, no call" $ map doTestN [2..7]
    , benchTestGroup "loop, no call" $ map doTestL [2..7]
    , benchTestGroup "no loop, call" $ map doTestC [2..4] -- Since we have call, loops are essentially enabled so we don't do 5,6,7
    , benchTestGroup "loop, call" $ map doTestLC [2..7] -- 2..4 are same as the <no loop, call> case; 5..7 are different
    ]
  ]
  where doTestN = simplePathTestN False "simple2_call" "foo"
        doTestL = simplePathTestL False "simple2_call" "foo"
        doTestC = simplePathTestC False "simple2_call" "foo"
        doTestLC = simplePathTestLC False "simple2_call" "foo"

simple2_call2_tests = benchTestGroup "simple2_call2.c" [
    benchTestGroup "getAllSimplePathsOfLength" [
      benchTestGroup "loop, call" $ map doTestLC [2..4]
    ]
  ]
  where doTestLC = simplePathTestLC False "simple2_call2" "foo"

-- simple_phi.ll

simple_phi_tests = benchTestGroup "simple_phi.ll" [
    benchTestGroup "getAllBasicBlockPathsOfLength" [
        bbTest 1 [ [2], {- useless bb [4], -} [5] ]
      , bbTest 2 [ [2, 4], [2, 5], [4, 5] ]
      , bbTest 3 [ [2, 4, 5] ]
    ]
  , benchTestGroup "getAllSimplePathsOfLength" $ map doTestN [1..3]
  ]
  where bbTest = bbPathTest "simple_phi" "foo"
        doTestN = simplePathTestN False "simple_phi" "foo"

loop_tests = benchTestGroup "loop.ll" [
    benchTestGroup "getAllBasicBlockPathsOfLength" [
        bbTestL 1 [ {- useless bb [1], -} [2], [9] ]
      , bbTestL 2 [ [1, 2]
                  , [2, 2], [2, 9] ]
      , bbTestL 3 [ [1, 2, 9]
                  , [1, 2, 2]
                  , [2, 2, 2]
                  , [2, 2, 9] ]
      , bbTestL 4 [ [1, 2, 2, 9]
                  , [1, 2, 2, 2]
                  , [2, 2, 2, 2]
                  , [2, 2, 2, 9] ]
    ],
    benchTestGroup "getAllSimplePathsOfLength" [
      simplePathTestL False "loop" "myFree" 4
    , simplePathTestL False "loop" "myFree" 5
    ]
  ]
  where bbTestL = bbPathTestL "loop" "myFree"

multipleCallers_tests = benchTestGroup "multipleCallers.ll" [
    benchTestGroup "return to correct caller" $
      [testNoFollowRets "caller1" 5, testNoFollowRets "caller2" 5]
  , benchTestGroup "multi-level correct caller" $
      [testNoFollowRets "callercaller" 18]
  , benchTestGroup "find any caller" $
      [testWithFollowRets "foo" 4]
  ]
  where testNoFollowRets = simplePathTestNoFollowRets True "multipleCallers"
        testWithFollowRets = simplePathTestLC True "multipleCallers"

recursive_tests = benchTestGroup "recursiveCalls.ll" [
    benchTestGroup "without entering" $ map (doTestL "simple") [1..7]
  , benchTestGroup "simple" $ map (doTestLC "simple") [1..7]
  , benchTestGroup "more blocks" $ map (doTestLC "moreBlocks") [1..7]
  , benchTestGroup "not tail rec" $ map (doTestLC "notTailRec") [1..7]
  , benchTestGroup "mutually" $ map (doTestLC "mutRecursiveA") [1..7]
  , benchTestGroup "3-way mutually" $ map (doTestLC "threeWayMutRecursiveA") [1..7]
  , benchTestGroup "nested" $ map (doTestLC "nestedRecursive") [1..7]
  , benchTestGroup "double" $ map (doTestLC "doubleRecursive") [1..7]
  ]
  where doTestL = simplePathTestL True "recursiveCalls"
        doTestLC = simplePathTestLC True "recursiveCalls"

switch_tests = benchTestGroup "switch-O2.ll" [
    benchTestGroup "switch-O2" $ map (doTestLC "foo") [1..3]
  ]
  where doTestLC = simplePathTestLC True "switch-O2"


filterPrefix_tests = benchTestGroup "filterPrefix tests" [
  benchTestGroup "no paths" [
      doSimpleTest [100] 1 []  -- BB 100 does not exist
    , doSimpleTest [100] 2 []
    , doSimpleTest [100] 3 []
    , doSimpleTest [100] 4 []
    , doSimpleTest [3, 10] 2 []  -- BB 10 does not exist
    , doSimpleTest [3, 10] 3 []
    , doSimpleTest [3, 9] 2 []  -- BBs 3 and 9 both exist, but no path between them
    , doSimpleTest [3, 9] 3 []
  ]
  , benchTestGroup "empty filter" [
      doSimpleTest [] 1 [1..5]
    , doSimpleTest [] 2 [1..6]
    , doSimpleTest [] 3 [1..4]
    , doSimpleTest [] 4 [1..2]
  ]
  , benchTestGroup "length-1 filters, length-1 paths" [
      doSimpleTest [3] 1 [1]
    , doSimpleTest [5] 1 [2]
    , doSimpleTest [7] 1 [3]
    , doSimpleTest [9] 1 [4]
    , doSimpleTest [12] 1 [5]
  ]
  , benchTestGroup "length-1 filters, length-2 paths" [
      doSimpleTest [3] 2 [1,2]
    , doSimpleTest [5] 2 [3,4]
    , doSimpleTest [7] 2 [5]
    , doSimpleTest [9] 2 [6]
    , doSimpleTest [12] 2 []
  ]
  , benchTestGroup "length-2 filters, length-2 paths" [
      doSimpleTest [3, 5] 2 [1]
    , doSimpleTest [3, 12] 2 [2]
    , doSimpleTest [7, 12] 2 [5]
    ]
  , benchTestGroup "length-4 filters, length-4 paths" [
      doSimpleTest [3,5,7,12] 4 [1]
    , doSimpleTest [3,5,9,12] 4 [2]
    ]
  , benchTestGroup "recursive: length-2 filters, length-5 paths" [
      doRecursiveTest [2, 12] 5 [1,2]
    , doRecursiveTest [16, 2] 5 [9..11]
    , doRecursiveTest [29, 16] 5 [12..15]
    ]
  , benchTestGroup "recursive: length-4 filters, length-5 paths" [
      doRecursiveTest [2,16,2,12] 5 [3]
    , doRecursiveTest [2,16,2,16] 5 [4]
    , doRecursiveTest [29,16,29,16] 5 [14,15]
    , doRecursiveTest [29,16,2,16] 5 [13]
  ] ]
  where doSimpleTest prefix =
          let prefixPairs = map (\num -> ("simple1.c", "foo", UnName num)) prefix
          in  filterPrefixTest False False False prefixPairs "simple1-O2" "foo"
        doRecursiveTest prefix =
          let prefixPairs = map (\num -> ("recursiveCalls.c", "doubleRecursive", UnName num)) prefix
          in  filterPrefixTest True True True prefixPairs "recursiveCalls" "doubleRecursive"

crossModule_tests = benchTestGroup "cross-module" [
    benchTestGroup "call and return" [
      crossModuleTestNoFollowRets "crossModule" ["simple1-O2"] "simpleCrossModuleCaller" 4
    , crossModuleTestNoFollowRets "crossModule" ["simple1-O2"] "multipleCrossModuleCaller" 4
    , crossModuleTestNoFollowRets "crossModule" ["multipleCallers"] "simpleCrossModuleCaller" 5
    , crossModuleTestNoFollowRets "crossModule" ["multipleCallers"] "multipleCrossModuleCaller" 5
    ]
  , benchTestGroup "with additional modules loaded" [
      crossModuleTestNoFollowRets "crossModule" ["recursiveCalls", "multipleCallers"] "simpleCrossModuleCaller" 5
    ]
  , benchTestGroup "call and return, 3 modules" [
      crossModuleTestNoFollowRets "crossModule2" ["simple1-O2", "crossModule"] "simple" 6
    , crossModuleTestNoFollowRets "crossModule2" ["crossModule", "multipleCallers"] "simple" 7
    ]
  , benchTestGroup "following rets across modules" [
      crossModuleTest "simple1-O2" ["crossModule"] "foo" 2
    , crossModuleTest "multipleCallers" ["crossModule"] "foo" 2
    , crossModuleTest "multipleCallers" ["crossModule", "crossModule2"] "foo" 3
    ]
  ]

--
-- Runners
--

-- | Test without loop
bbPathTest :: FilePath -> Name -> Int -> [[Word]] -> BenchTest
bbPathTest = bbPathTest_ False

-- | Test with loop
bbPathTestL :: FilePath -> Name -> Int -> [[Word]] -> BenchTest
bbPathTestL = bbPathTest_ True

-- | Actual tester for getAllBasicBlockPathsOfLength
bbPathTest_ :: Bool -> FilePath -> Name -> Int -> [[Word]] -> BenchTest
bbPathTest_ handleLoop file funcName len ex = benchTestCase (file ++ ":" ++ showFuncName funcName ++ " @ " ++ show len) $
 withModule (llvmPath ++ file ++ ".ll") $ \mInfo -> do
     paths0 <- getAllSimplePaths cfg funcName mInfo []
     let bbPaths = nub $ map (\sp -> map (getBlockName . spbeBB) $ bbs sp) paths0
     sort bbPaths @?= sort (map (map UnName) ex)
 where cfg = PathSearchCfg { psHandleLoops  = handleLoop
                           , psPathLength   = len
                           , psEnterCalls   = dontEnterCalls
                           , psFollowRets   = True
                           , psQuitOn       = dontQuit
                           , psFilterPrefix = S.empty
                           }

simplePathTestN :: Bool -> FilePath -> Name -> Int -> BenchTest
simplePathTestN = simplePathTest False False False "N"

simplePathTestL :: Bool -> FilePath -> Name -> Int -> BenchTest
simplePathTestL = simplePathTest True False True "L"

simplePathTestC :: Bool -> FilePath -> Name -> Int -> BenchTest
simplePathTestC = simplePathTest False True True "C"

simplePathTestLC :: Bool -> FilePath -> Name -> Int -> BenchTest
simplePathTestLC = simplePathTest True True True "LC"

simplePathTestNoFollowRets :: Bool -> FilePath -> Name -> Int -> BenchTest
simplePathTestNoFollowRets = simplePathTest True True False "LCn"

simplePathTest :: Bool -> Bool -> Bool -> String -> Bool -> FilePath -> Name -> Int -> BenchTest
simplePathTest handleLoop handleCall followRets chars includeTrackOps file funcName pathLength = simplePathTestWithCfg cfg chars includeTrackOps file [] funcName Nothing
 where cfg = PathSearchCfg { psHandleLoops  = handleLoop
                           , psPathLength   = pathLength
                           , psEnterCalls   = if handleCall then enterAllCalls else dontEnterCalls
                           , psFollowRets   = followRets
                           , psQuitOn       = dontQuit
                           , psFilterPrefix = S.empty
                           }

simplePathTestWithCfg :: PathSearchCfg -> String -> Bool
                      -> FilePath    -- ^ File containing the starting function
                      -> [FilePath]  -- ^ (Optional) additional files to follow paths into
                      -> Name        -- ^ Function name. We'll look for paths starting in this function
                      -> Maybe [Int] -- ^ Which golden-file numbers do we expect to see.
                                     -- E.g. [1,3] indicates we expect to see the paths in
                                     -- golden files 1 and 3. If this is Nothing, we expect
                                     -- to see the paths for all the golden files in the
                                     -- directory (the default)
                      -> BenchTest
simplePathTestWithCfg cfg chars includeTrackOps file xtraFiles funcName mExpectedPaths = benchTestCase (file ++ ":" ++ showFuncName funcName ++ " @ " ++ show (psPathLength cfg) ++ ", " ++ chars) $
  let llFile = llvmPath ++ file ++ ".ll"
      llXtraFiles = map (\f -> llvmPath ++ f ++ ".ll") xtraFiles
  in  withModuleAndContext llFile $ \ctx mInfo -> do
      xtraMods <- mapM (getModuleInfoFromFile ctx) llXtraFiles
      paths0 <- getAllSimplePaths cfg funcName mInfo xtraMods
      let paths1 = if includeTrackOps then paths0
                                      else map (\sp -> sp { instrs = filter isOkInstr $ instrs sp }) paths0
          dir = goldenDir cfg file xtraFiles funcName
      generateGoldenFiles <- isJust <$> lookupEnv "GENERATE"  -- if the environment variable "GENERATE" is defined at all, then we are generating golden files
      when generateGoldenFiles $ do
        when (isJust mExpectedPaths) $ assertFailure "Tests with active filterPrefixes do not run when we are generating golden files"  -- don't generate golden files from any test with an active filterPrefix. In fact, we can't even run these tests, because we can't guarantee that they run *after* the test that generates the golden files they need
        removePathForcibly dir
        createDirectoryIfMissing True dir  -- creates parents too, if needed
        let sortedPaths = sortOn bbNamesThenInstructions paths1
        zipWithM_ (\num path -> T.writeFile (dir ++ "/" ++ show num) path) [1..] $ map showGolden sortedPaths
        -- now the rest of the test continues for sanity check (it should pass by definition)
      expectedPaths <- case mExpectedPaths of
        Just paths -> return paths
        Nothing    -> catch (map read <$> listDirectory dir)
                            (\e -> if isDoesNotExistError (e :: IOError)
                                      then return []  -- if the dir does not exist, we expect the test to return no paths
                                      else error $ "Error listing directory " ++ show dir ++ ": " ++ show e
                            )
      length paths1 @?= length expectedPaths
      let goldenFileNames = map (\num -> dir ++ "/" ++ show num) expectedPaths
      goldenFiles <- mapM T.readFile goldenFileNames
      let goldens = S.fromList goldenFiles
          observeds = S.fromList $ map showGolden paths1
          goldens' = S.map (T.replace " " "") goldens  -- so whitespace doesn't matter in the comparison
          observeds' = S.map (T.replace " " "") observeds  -- so whitespace doesn't matter in the comparison
      -- putStrLn $ "goldenFileName = " ++ show goldenFileNames
      -- putStrLn $ "observeds =" ++ show observeds
      -- putStrLn $ "goldens =" ++ show goldens
      when (goldens' /= observeds') $ do
        -- XXX: this is ugly to leave here, but useful when debugging
        -- T.writeFile "/tmp/gold" $ T.unlines goldenFiles  -- leaves the whitespace in, which is much more readable
        -- T.writeFile "/tmp/got" $ T.unlines (map showGolden $ sortOn pathToListOfBBNames paths1)
        assertFailure $ "these golden paths: " ++ (show $ S.elems $ goldens' S.\\ observeds') ++
                        "\ndid not match these observed paths: " ++ (show $ S.elems $ observeds' S.\\ goldens') ++
                        "\nGolden paths written to /tmp/gold, and observed paths written to /tmp/got"
          where bbNamesThenInstructions :: SimplePath -> ([Name], [String])
                bbNamesThenInstructions spath = (pathToListOfBBNames spath, map show $ instrs spath)
                pathToListOfBBNames :: SimplePath -> [Name]
                pathToListOfBBNames spath = map (getBlockName . spbeBB) $ bbs spath
                isOkInstr i = isSimpleInstr i || isPathEq i || isParEqs i

filterPrefixTest :: Bool       -- ^ Handle loop?
                 -> Bool       -- ^ Handle call?
                 -> Bool       -- ^ Include track ops?
                 -> PathPrefix -- ^ Filter prefix
                 -> FilePath   -- ^ File
                 -> Name       -- ^ Function
                 -> Int        -- ^ Path length
                 -> [Int]      -- ^ Which golden-file numbers do we expect to see.
                               -- E.g. [1,3] indicates we expect to see the paths in
                               -- golden files 1 and 3.
                 -> BenchTest
filterPrefixTest handleLoop handleCall includeTrackOps filterPrefix file funcName pathLength expectedPaths =
  simplePathTestWithCfg cfg lcStr includeTrackOps file [] funcName $ Just expectedPaths
    where cfg = PathSearchCfg { psHandleLoops  = handleLoop
                              , psPathLength   = pathLength
                              , psEnterCalls   = if handleCall then enterAllCalls else dontEnterCalls
                              , psFollowRets   = True
                              , psQuitOn       = dontQuit
                              , psFilterPrefix = if null filterPrefix
                                                   then S.empty
                                                   else S.singleton filterPrefix
                              }
          lcStr = case () of
                  _ | handleLoop && handleCall -> "LC"
                  _ | handleLoop -> "L"
                  _ | handleCall -> "C"
                  _ -> "N"

crossModuleTest :: FilePath -> [FilePath] -> Name -> Int -> BenchTest
crossModuleTest file xtraFiles funcName pathLength =
  simplePathTestWithCfg cfg "LC" True file xtraFiles funcName Nothing
  where cfg = PathSearchCfg { psHandleLoops  = True
                            , psPathLength   = pathLength
                            , psEnterCalls   = enterAllCalls
                            , psFollowRets   = True
                            , psQuitOn       = dontQuit
                            , psFilterPrefix = S.empty
                            }

crossModuleTestNoFollowRets :: FilePath -> [FilePath] -> Name -> Int -> BenchTest
crossModuleTestNoFollowRets file xtraFiles funcName pathLength =
  simplePathTestWithCfg cfg "LCn" True file xtraFiles funcName Nothing
  where cfg = PathSearchCfg { psHandleLoops  = True
                            , psPathLength   = pathLength
                            , psEnterCalls   = enterAllCalls
                            , psFollowRets   = False
                            , psQuitOn       = dontQuit
                            , psFilterPrefix = S.empty
                            }

-- Alternate Show instance for Name which doesn't include the string "Name" all the time
showFuncName :: Name -> String
showFuncName (Name s) = toString $ fromShort s
showFuncName (UnName w) = show w

-- Alternate Show instance for SimplePath meant to produce the same format as our golden files.
-- Leaves whitespace alone for now; later we will strip whitespace to do the comparison
showGolden :: SimplePath -> Text
showGolden (SimplePath is _) = T.intercalate "\n" (map (T.pack . show) is) `T.append` "\n"

-- | Get the name of the directory wherein to store golden files for the given config, file stem, and function name
goldenDir :: PathSearchCfg -> FilePath -> [FilePath] -> Name -> String
goldenDir cfg fileName xtraFiles funcName =
  "test/Pathsearch/paths/" ++ fileName ++ "/" ++ maybeLinked ++ showFuncName funcName ++ "/" ++ show (psPathLength cfg) ++ "/" ++ loop ++ "." ++ call
  where loop = if psHandleLoops cfg then "loop" else "no-loop"
        call = if psEnterCalls cfg == enterAllCalls then "call"
               else if psEnterCalls cfg == dontEnterCalls then "no-call"
               else "some-call"
        maybeLinked :: String
        maybeLinked = if null xtraFiles then ""
                      else (intercalate "-" $ "linked":xtraFiles) ++ "/"
