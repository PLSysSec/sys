{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module PathPrefix ( pathPrefixTests ) where

import           Data.ByteString.UTF8 (toString)
import           Data.ByteString.Short (fromShort)
import qualified Data.Set as Set

import           LLVM.AST hiding (nsw, nuw)
import           LLVM.Prelude hiding (void)
import           LLVMAST.ASTInterface
import           InternalIR.SimplePath
import           Utils.Pathsearch.Config
import           Utils.Pathsearch.Pathsearch

import           Test.Tasty.HUnit

import           Utils

default (Integer, ShortByteString)

llvmPath :: String
llvmPath = "test/Pathsearch/llvm/"

pathPrefixTests :: BenchTest
pathPrefixTests = benchTestGroup "PathPrefix" [
    benchTestGroup "getAllPathPrefixesWithSuffix" [
      pfx_simple1_tests
      , pfx_simple2_tests
      , pfx_simple2_call_tests
      , pfx_loop_tests
    ]
    , benchTestGroup "getAllSimplePaths" [
      paths_simple1_tests
    , paths_simple2_tests
    , paths_simple2_call_tests
  ]
  ]


--
-- path prefix
--

pfx_simple1_tests = benchTestGroup "simple1.ll" [
  -- one block
    test 1 13 [[13]]
  , test 1 25 [[25]]
  -- two blocks
  , test 2 13 [[3, 13]]
  , test 2 25 [[19, 25], [16, 25], [12, 25]]
  -- three blocks
  , test 3 13 [[3, 13]]
  , test 3 16 [[3, 13, 16]]
  , test 3 25 [[13, 19, 25], [13, 16, 25], [3, 12, 25]]
  -- four blocks
  , test 4 13 [[3, 13]]
  , test 4 16 [[3, 13, 16]]
  , test 4 25 [[3, 13, 19, 25], [3, 13, 16, 25], [3, 12, 25]]
  ]
  where test = bbPathTest "simple1" "foo"

pfx_simple2_tests = benchTestGroup "simple2.ll" [
  -- three, starting beyond phi
    test 3 12 [[17, 8, 12], [3, 8, 12]]
  -- four blocks
  , test 4 17 [[17, 8, 12, 17], [3, 8, 12, 17]]
  ]
  where test = bbPathTest "simple2" "foo"

pfx_simple2_call_tests = benchTestGroup "simple2_call.ll" [
  -- three, starting beyond phi
    test 3 17 [[8, 12, 17]]
  ]
  where test = bbPathTest "simple2_call" "foo"

pfx_loop_tests = benchTestGroup "loop.ll" [
  -- two
    test 2 2 [[2, 2], [1, 2]]
  , test 2 9 [[2, 9]]
  -- three
  , test 3 2 [[1, 2], [2, 2, 2], [1, 2, 2]]
  , test 3 9 [[2, 2, 9], [1, 2, 9]]
  -- four
  , test 4 2 [[1, 2], [2, 2, 2, 2], [1, 2, 2], [1, 2, 2, 2]]
  , test 4 9 [[2, 2, 2, 9], [1, 2, 2, 9], [1, 2, 9]]
  ]
  where test = bbPathTest "loop" "myFree"

--
-- path search tests
--



paths_simple1_tests = benchTestGroup "simple1.ll" [

    test (1,3) 13 [[3, 13, 16], [3, 13, 19]]
  , test (2,3) 13 [[3, 13, 16], [3, 13, 19]] -- can't go back further
  , test (2,4) 13 [[3, 13, 16, 25], [3, 13, 19, 25]]

  , test (2,3) 19 [[3, 13, 19]]
  , test (2,3) 19 [[3, 13, 19]]

  , test (1,4) 12 []
  ]
  where test :: (Int, Int) -> Word -> [[Word]] -> BenchTest
        test = twBBPathTest "simple1" "foo"

paths_simple2_tests = benchTestGroup "simple2.ll" [

    test (1,3) 8 [ [17, 8, 12]
                 , [17, 8, 20]
                 , [3,  8, 12]
                 , [3,  8, 20 ] ]

  , test (2,3) 8 [ [12, 17, 8]
                 , [3,  8, 12] -- hits top
                 , [3,  8, 20] -- hits top
                 ]

  , test (3,5) 17 [ [17, 8, 12, 17, 8]
                  , [3,  8, 12, 17, 8] ]

  , test (4,6) 17 [ [12, 17, 8, 12, 17, 8]
                  , [3,  8, 12, 17, 8, 12] -- hits top
                  , [3,  8, 12, 17, 8, 20] -- hits top
                  ]
  ]
  where test :: (Int, Int) -> Word -> [[Word]] -> BenchTest
        test = twBBPathTest "simple2" "foo"

paths_simple2_call_tests = benchTestGroup "simple2_call.ll" [
    test (1,3) 8 [ [("foo", 17), ("foo", 8), ("foo", 12)]
                 , [("foo", 17), ("foo", 8), ("foo", 20)]
                 , [("foo", 3),  ("foo", 8), ("foo", 12)]
                 , [("foo", 3),  ("foo", 8), ("foo", 20)] ]

  , test (1,4) 8 [ [("foo", 17), ("foo", 8), ("foo", 12), ("bar", 2)]
                 , [("foo", 3),  ("foo", 8), ("foo", 12), ("bar", 2)]
                 ]

  , test (1,5) 8 [ [("foo", 17), ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12)]
                 , [("foo", 3),  ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12)] ]

  , test (1,6) 8 [ [("foo", 17), ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12), ("foo", 17)]
                 , [("foo", 3),  ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12), ("foo", 17)] ]

  , test (1,7) 8 [ [("foo", 17), ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12), ("foo", 17), ("foo", 8)]
                 , [("foo", 3),  ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12), ("foo", 17), ("foo", 8)] ]


  , test (1,3) 12 [ [("foo", 8), ("foo", 12), ("bar", 2)] ]
  -- dont enter call in 12
  , test (2,3) 17 [ [("foo", 8), ("foo", 12), ("foo", 17)] ]

  , test (2,5) 17 [ [("foo", 8), ("foo", 12), ("foo", 17), ("foo", 8), ("foo", 12)]
                  , [("foo", 8), ("foo", 12), ("foo", 17), ("foo", 8), ("foo", 20)]
                  ]

  -- dont enter call before, do enter after
  , test (2,6) 17 [ [("foo", 8), ("foo", 12), ("foo", 17), ("foo", 8), ("foo", 12), ("bar", 2)] ]
  , test (2,8) 17 [ [("foo", 8), ("foo", 12), ("foo", 17), ("foo", 8), ("foo", 12), ("bar", 2), ("foo", 12), ("foo", 17)] ]
  ]
  where test :: (Int, Int) -> Word -> [[(Name, Word)]] -> BenchTest
        test = twBBPathTest "simple2_call" "foo"

--
-- runners
--


-- | Actual tester
bbPathTest :: String -> Name -> Int -> Word -> [[Word]] -> BenchTest
bbPathTest file funcName len bbNr ex = benchTestCase testName $
 withModule (llvmPath ++ file ++ ".ll") $ \mInfo -> do
    let got = getAllPathPrefixesWithSuffix funcName mInfo len bPred
    got @?= Set.fromList (map (map (\nr -> (file ++ ".c", funcName, UnName nr))) ex)
  where testName = file ++ ":" ++ showName funcName ++
                   " len = " ++ show len ++ " end =  " ++ show bbNr
        bPred = BlockPredicate $ \_ bb -> getBlockName bb == UnName bbNr

-- Alternate Show instance for Name which doesn't include the string "Name" all the time
showName :: Name -> String
showName (Name s)   = toString $ fromShort s
showName (UnName w) = show w


class TwBBPathTest a where
  twBBPathTest :: String -> Name -> (Int, Int) -> Word -> [[a]] -> BenchTest

instance TwBBPathTest Word where
  twBBPathTest file funcName (back, len) bbNr ex =
    twBBPathTest file funcName (back, len) bbNr $ map (map (\nr -> (file ++ ".c", funcName, UnName nr))) ex

instance TwBBPathTest (Name, Word) where
  twBBPathTest file funcName (back, len) bbNr ex =
    twBBPathTest file funcName (back, len) bbNr $ map (map (\(f, nr) -> (file ++ ".c", f, UnName nr))) ex

instance TwBBPathTest (Name, Name) where
  twBBPathTest file funcName (back, len) bbNr ex =
    twBBPathTest file funcName (back, len) bbNr $ map (map (\(f, bbname) -> (file ++ ".c", f, bbname))) ex

instance TwBBPathTest (String, Name, Name) where
  twBBPathTest file funcName (back, len) bbNr ex = benchTestCase testName $
    withModule (llvmPath ++ file ++ ".ll") $ \modInfo -> do
        let cfg1 = toTargettedWindowCfg funcName modInfo cfg0 twCfg
        paths0 <- getAllSimplePaths cfg1 funcName modInfo []
        let bbPaths = Set.fromList $ map (\sp -> map asNames $ bbs sp) paths0
        bbPaths @?= Set.fromList ex
          where cfg0 = PathSearchCfg { psHandleLoops  = True
                                     , psPathLength   = len
                                     , psEnterCalls   = enterAllCalls
                                     , psFollowRets   = True
                                     , psQuitOn       = dontQuit
                                     , psFilterPrefix = Set.empty
                                     }
                twCfg = TargettedWindowCfg { swMustInclude = BlockPredicate $ \_ bb -> getBlockName bb == UnName bbNr
                                           , swGoBackBy     = back }
                testName = file ++ ":" ++ showName funcName ++
                            " @ " ++ show (back, len) ++ " includes " ++ show bbNr
