{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module SSA ( ssaTests ) where

import           LLVM.AST                  hiding (nsw, nuw)
import           LLVM.Prelude              hiding (void)
import           InternalIR.SimplePath

import           Test.Tasty.HUnit

import           InternalIR.SSA
import           TestingDSL
import           Utils

default (Integer, ShortByteString)

ssaTests :: BenchTest
ssaTests = benchTestGroup "RenamePath" [ recursiveCalls_tests
                                       , nodeuaffp2_tests
                                       , dontRenameEq_test
                                       , self_assignment_renames_test
                                       , account_for_diff_paths_test
                                       , renamePar_test
                                       , sanity_check_1
                                       , sanity_check_2
                                       ]
--
-- recursiveCalls.ll
--

recursiveCalls_tests = benchTestGroup "recursiveCalls.ll" [
    benchTestCase "0,9,14"  $ simple1 @=? toSSA simple1
  , benchTestCase "0,11,14" $ simple2 @=? toSSA simple2
  , benchTestCase "0,11a,0',9',14',11b,14" $ simple3' @=? toSSA simple3
  ]

simple1 :: SimplePath
simple1 = SimplePath { bbs = [], instrs =
  simple_bb0 ++ simple_bb9 ++ simple_bb14
  }

simple2 :: SimplePath
simple2 = SimplePath { bbs = [], instrs =
  simple_bb0 ++ simple_bb11 ++ simple_bb14
  }

simple3 :: SimplePath
simple3 = SimplePath { bbs = [], instrs = simple3instr }

simple3' :: SimplePath
simple3' = SimplePath { bbs = [], instrs = simple3'instr }

simple_bb0 :: [SimpleInstruction]
simple_bb0 =
  [ Instr $ "simple_2" := alloca i32 4
  , Instr $ "simple_3" := alloca i32 4
  , Instr $ "simple_4" := alloca i32 4
  , Instr $ Do $ store i32 "simple_0" "simple_3" 4
  , Instr $ "simple_5" := load i32 "simple_3" 4
  , Instr $ "simple_6" := add nsw i32 "simple_5"  2
  , Instr $ Do $ store i32 "simple_6" "simple_4" 4
  , Instr $ "simple_7" := load i32 "simple_4" 4
  , Instr $ "simple_8" := icmp sgt i32 "simple_7" 25
  , TrackOps [ r i1 "simple_8" ] ]

simple_bb9 :: [SimpleInstruction]
simple_bb9 =
  [ Instr $ "simple_10" := load i32 "simple_4" 4
  , Instr $ Do $ store i32 "simple_10" "simple_2" 4 ]

simple_bb11 :: [SimpleInstruction]
simple_bb11 =
  [ Instr $ "simple_12" := load i32 "simple_4" 4
  , Instr $ "simple_13" := call Nothing ccc i32 "simple" [r i32 "simple_12"]
  , Instr $ Do $ store i32 "simple_13" "simple_2" 4 ]

simple_bb14 :: [SimpleInstruction]
simple_bb14 =
  [ Instr $ "simple_15" := load i32 "simple_2" 4
  , TrackOps [ r i32 "simple_15" ] ]

--
-- for the call case
--

simple3instr :: [SimpleInstruction]
simple3instr =
  -- simple_bb0:
  [ Instr $ "simple_2" := alloca i32 4
  , Instr $ "simple_3" := alloca i32 4
  , Instr $ "simple_4" := alloca i32 4
  , Instr $ Do $ store i32 "simple_0" "simple_3" 4
  , Instr $ "simple_5" := load i32 "simple_3" 4
  , Instr $ "simple_6" := add nsw i32 "simple_5"  2
  , Instr $ Do $ store i32 "simple_6" "simple_4" 4
  , Instr $ "simple_7" := load i32 "simple_4" 4
  , Instr $ "simple_8" := icmp sgt i32 "simple_7" 25
  , TrackOps [ r i1 "simple_8" ] ] ++
  -- simple_bb11a:
  [ Instr $ "simple_12" := load i32 "simple_4" 4
  , Instr $ "simple_0"  := bitcast i32 "simple_12" i32 ] ++
  -- <call>
  [ EnterCall "simple" ] ++
    -- simple_bb0:
    [ Instr $ "simple_2" := alloca i32 4
    , Instr $ "simple_3" := alloca i32 4
    , Instr $ "simple_4" := alloca i32 4
    , Instr $ Do $ store i32 "simple_0" "simple_3" 4
    , Instr $ "simple_5" := load i32 "simple_3" 4
    , Instr $ "simple_6" := add nsw i32 "simple_5"  2
    , Instr $ Do $ store i32 "simple_6" "simple_4" 4
    , Instr $ "simple_7" := load i32 "simple_4" 4
    , Instr $ "simple_8" := icmp sgt i32 "simple_7" 25
    , TrackOps [ r i1 "simple_8" ] ] ++
    -- simple_bb9:
    [ Instr $ "simple_10" := load i32 "simple_4" 4
    , Instr $ Do $ store i32 "simple_10" "simple_2" 4 ] ++
    -- simple_b14:
    [ Instr $ "simple_15" := load i32 "simple_2" 4
    , TrackOps [ r i32 "simple_15" ] ] ++
  [ ExitCall (Just "simple_13")] ++
  -- </call>
  -- simple_bb11b:
  [ Instr $ "simple_13" := bitcast i32 "simple_15" i32
  , Instr $ Do $ store i32 "simple_13" "simple_2" 4 ] ++
  -- simple_bb14:
  [ Instr $ "simple_15" := load i32 "simple_2" 4
  , TrackOps [ r i32 "simple_15" ] ]

simple3'instr :: [SimpleInstruction]
simple3'instr =
  -- simple_bb0:
  [ Instr $ "simple_2" := alloca i32 4
  , Instr $ "simple_3" := alloca i32 4
  , Instr $ "simple_4" := alloca i32 4
  , Instr $ Do $ store i32 "simple_0" "simple_3" 4
  , Instr $ "simple_5" := load i32 "simple_3" 4
  , Instr $ "simple_6" := add nsw i32 "simple_5"  2
  , Instr $ Do $ store i32 "simple_6" "simple_4" 4
  , Instr $ "simple_7" := load i32 "simple_4" 4
  , Instr $ "simple_8" := icmp sgt i32 "simple_7" 25
  , TrackOps [ r i1 "simple_8" ] ] ++
  -- simple_bb11a:
  [ Instr $ "simple_12" := load i32 "simple_4" 4
  , Instr $ "simple_0_1" := bitcast i32 "simple_12" i32 ] ++
  -- <call>
  [ EnterCall "simple" ] ++
    -- simple_bb0:
    [ Instr $ "simple_2_1" := alloca i32 4
    , Instr $ "simple_3_1" := alloca i32 4
    , Instr $ "simple_4_1" := alloca i32 4
    , Instr $ Do $ store i32 "simple_0_1" "simple_3_1" 4
    , Instr $ "simple_5_1" := load i32 "simple_3_1" 4
    , Instr $ "simple_6_1" := add nsw i32 "simple_5_1"  2
    , Instr $ Do $ store i32 "simple_6_1" "simple_4_1" 4
    , Instr $ "simple_7_1" := load i32 "simple_4_1" 4
    , Instr $ "simple_8_1" := icmp sgt i32 "simple_7_1" 25
    , TrackOps [ r i1 "simple_8_1" ] ] ++
    -- simple_bb9:
    [ Instr $ "simple_10" := load i32 "simple_4_1" 4
    , Instr $ Do $ store i32 "simple_10" "simple_2_1" 4 ] ++
    -- simple_b14:
    [ Instr $ "simple_15" := load i32 "simple_2_1" 4
    , TrackOps [ r i32 "simple_15" ] ] ++
  [ ExitCall (Just "simple_13")] ++
  -- -- </call>
  -- -- simple_bb11b:
  [ Instr $ "simple_13" := bitcast i32 "simple_15" i32
  , Instr $ Do $ store i32 "simple_13" "simple_2" 4 ] ++ -- dumb renaming thinks it's 2_1, but should be 2
  -- simple_bb14:
  [ Instr $ "simple_15_1" := load i32 "simple_2" 4       -- dumb renaming thinks it's 2_1, but should be 2
  , TrackOps [ r i32 "simple_15_1" ] ]

--
-- nodeuaffp2.ll
--

nodeuaffp2_tests = benchTestGroup "nodeuaffp2.ll" [
    benchTestCase "loopmiddle,loopend,loopstart"  $ uaf1' @=? toSSA uaf1
  ]

uaf1 :: SimplePath
uaf1 = SimplePath { bbs = [], instrs =
  -- myFree_loopmiddle
  [ Instr $ Do $ tail'call ccc void "free" [r i8' "myFree_nextpointer"]
  , Instr $ "myFree_result" := load i8 "myFree_nextpointer" 1
  , Instr $ "myFree_dep" := add nuw i8 "myFree_result" "myFree_result" ] ++
  -- myFree_loopend
  -- br loopstart
  -- myFree_loopstart
  [ Instr $ "myFree_pointer" := bitcast i8' "myFree_nextpointer" i8'
  , Instr $ "myFree_nextpointer" := getelementptr True i8' "myFree_pointer" [ci i32 1] ]
  }

uaf1' :: SimplePath
uaf1' = SimplePath { bbs = [], instrs =
  -- myFree_loopmiddle
  [ Instr $ Do $ tail'call ccc void "free" [r i8' "myFree_nextpointer"]
  , Instr $ "myFree_result" := load i8 "myFree_nextpointer" 1
  , Instr $ "myFree_dep" := add nuw i8 "myFree_result" "myFree_result" ] ++
  -- myFree_loopend
  -- br loopstart
  -- myFree_loopstart
  [ Instr $ "myFree_pointer" := bitcast i8' "myFree_nextpointer" i8'
  , Instr $ "myFree_nextpointer_1" := getelementptr True i8' "myFree_pointer" [ci i32 1] ]
  }


dontRenameEq_test = benchTestCase "dont rename eq"  $ p' @=? toSSA p
  where p = SimplePath { bbs = []
                       , instrs  = [ Instr $ "simple_1"  := bitcast i1 "simple_0" i1
                                   , PathEq $ "simple_1"  := bitcast i1 1 i1 ] }
        p' = SimplePath { bbs = []
                        , instrs  = [ Instr $ "simple_1"  := bitcast i1 "simple_0" i1
                                    , PathEq $ "simple_1"  := bitcast i1 1 i1 ] }

renamePar_test = benchTestCase "handle renames between branches"  $ p' @=? toSSA p
  where p = SimplePath { bbs = []
                       , instrs  = phisToPar ["x" := phi i32 [("y", "one"), ("z", "two")]
                                             ,"y" := phi i32 [("z", "one"), ("x", "two")]] :
                                   [ Instr $ "u" := bitcast i32 "x" i32
                                   , Instr $ "w" := bitcast i32 "y" i32 ] }
        p' = SimplePath { bbs = []
                        , instrs  = [ ParEqs [["x" := bitcast i32 "y" i32, "y_1" := bitcast i32 "z" i32]
                                             ,["x" := bitcast i32 "z" i32, "y" := bitcast i32 "x" i32, "y_1" := bitcast i32 "y" i32]]
                                    , Instr $ "u" := bitcast i32 "x" i32
                                    , Instr $ "w" := bitcast i32 "y_1" i32 ] }

-- pretty much same as renamePar_test but more direct
account_for_diff_paths_test = benchTestCase "account for vars in diff path"  $ p1 @=? toSSA p0
  where p0 = SimplePath { bbs = []
                        , instrs  = [ ParEqs [["x" := bitcast i32 "y" i32, "y" := bitcast i32 "z" i32]
                                             ,["x" := bitcast i32 "z" i32, "y" := bitcast i32 "x" i32]]
                                    ] }
        p1 = SimplePath { bbs = []
                        , instrs  = [ ParEqs [["x" := bitcast i32 "y" i32, "y_1" := bitcast i32 "z" i32]
                                             ,["x" := bitcast i32 "z" i32, "y" := bitcast i32 "x" i32, "y_1" := bitcast i32 "y" i32]]
                                    ] }

self_assignment_renames_test = benchTestCase "self assignment renames"  $ p1 @=? toSSA p0
  where p0 = SimplePath { bbs = []
                        , instrs  = [ Instr $ "x" := bitcast i32 "z" i32
                                    , Instr $ "y" := bitcast i32 "x" i32
                                    , Instr $ "y" := bitcast i32 "y" i32] }
        p1 = SimplePath { bbs = []
                        , instrs  = [ Instr $ "x" := bitcast i32 "z" i32
                                    , Instr $ "y" := bitcast i32 "x" i32
                                    , Instr $ "y_1" := bitcast i32 "y" i32] }

sanity_check_1 = benchTestCase "sanity check test 1"  $ p @=? toSSA p
  where p = SimplePath { bbs = []
                       , instrs  = [ Instr $ "x" := bitcast i32 "z" i32
                                   , Instr $ "y" := bitcast i32 "x" i32
                                   , Instr $ "y_1" := bitcast i32 "y" i32] }

sanity_check_2 = benchTestCase "sanity check test 2"  $ p1 @=? toSSA p0
  where p0 = SimplePath { bbs = []
                        , instrs  = [ ParEqs [["x" := bitcast i32 "y" i32, "y" := bitcast i32 "z" i32]
                                             ,["x" := bitcast i32 "z" i32, "y" := bitcast i32 "x" i32, "y" := bitcast i32 "y" i32]]
                                    ] }
        p1 = SimplePath { bbs = []
                        , instrs  = [ ParEqs [["x" := bitcast i32 "y" i32, "y_1" := bitcast i32 "z" i32]
                                             ,["x" := bitcast i32 "z" i32, "y" := bitcast i32 "x" i32, "y_1" := bitcast i32 "y" i32]]
                                    ] }

