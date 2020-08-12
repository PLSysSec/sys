{-# LANGUAGE OverloadedStrings #-}
module StaticPath (staticPathTests) where

import           Control.Monad             (when)
import           Data.Maybe                (fromJust, isJust)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           LLVM.AST
import           LLVM.Context
import           LLVMAST.ASTInterface
import           InternalIR.ModuleInfo
import           InternalIR.SimplePath
import           System.Environment        (lookupEnv)
import           Test.Tasty.HUnit
import           Utils
import           Utils.Pathsearch.StaticPath

staticPathTests :: BenchTest
staticPathTests = benchTestGroup "StaticPath" [ oneblocktests
                                              , twoblocktests
                                              , moreblocktests
                                              , twoblockCallTests
                                              , moreblockCallTests
                                              , multipleCallsInBlockTests
                                              , multipleCallersTests
                                              , recursiveTests
                                              , crossModuleTests
                                              ]

oneblocktests :: BenchTest
oneblocktests = benchTestGroup "one block" [
    staticPathTest "simple1-3" "simple1" [("foo", 3, NotViaCall)]
  , staticPathTest "simple1-12" "simple1" [("foo", 12, NotViaCall)]
  , staticPathTest "simple1-25" "simple1" [("foo", 25, NotViaCall)]
  , staticPathTest "simple1-O2-3" "simple1-O2" [("foo", 3, NotViaCall)]
  , staticPathTest "simple1-O2-9" "simple1-O2" [("foo", 9, NotViaCall)]
  , staticPathTest "simple1-O2-12" "simple1-O2" [("foo", 12, NotViaCall)]
  , staticPathTest "not-entering-call" "simple2_call" [("foo", 12, NotViaCall)]
  , staticPathTest "entering-call" "simple2_call" [("foo", 12, ViaCall "bar")]
  ]

twoblocktests :: BenchTest
twoblocktests = benchTestGroup "two blocks" [
    staticPathTest "uncondbranch" "simple1" [ ("foo", 12, NotViaCall)
                                            , ("foo", 25, NotViaCall)
                                            ]
  , staticPathTest "condbranch-true" "simple1" [ ("foo", 3, NotViaCall)
                                               , ("foo", 12, NotViaCall)
                                               ]
  , staticPathTest "condbranch-false" "simple1" [ ("foo", 3, NotViaCall)
                                                , ("foo", 13, NotViaCall)
                                                ]
    -- can't branch to a block other than the one specified
  , expectNoPath "uncondbranch-invalid" "simple1" [ ("foo", 12, NotViaCall)
                                                  , ("foo", 13, NotViaCall)
                                                  ]
    -- can't branch to a block other than the two specified
  , expectNoPath "condbranch-invalid" "simple1" [ ("foo", 3, NotViaCall)
                                                , ("foo", 25, NotViaCall)
                                                ]
  ]

moreblocktests :: BenchTest
moreblocktests = benchTestGroup "more blocks" [
    staticPathTest "simple1-foo" "simple1" $
      bbSequence [3, 13, 16, 25]
  , staticPathTest "with-a-loop" "simple2" $
      bbSequence [3, 8, 12, 17, 8, 12, 17, 8, 20]
  ]
  where bbSequence = map (\n -> ("foo", n, NotViaCall))

twoblockCallTests :: BenchTest
twoblockCallTests = benchTestGroup "two blocks, calls" [
    -- don't enter the call
    staticPathTest "not-calling" "simple2_call" [ ("foo", 12, NotViaCall)
                                                , ("foo", 17, NotViaCall)
                                                ]
    -- can't take the call when we said not to
  , expectNoPath "not-calling-invalid" "simple2_call" [ ("foo", 12, NotViaCall)
                                                      , ("bar", 2, NotViaCall)
                                                      ]
    -- enter the call
  , staticPathTest "calling" "simple2_call" [ ("foo", 12, ViaCall "bar")
                                            , ("bar", 2, NotViaCall)
                                            ]
    -- can't not take the call when we said to
  , expectNoPath "calling-invalid" "simple2_call" [ ("foo", 12, ViaCall "bar")
                                                  , ("foo", 17, NotViaCall)
                                                  ]
    -- can't enter a random place in the callee
  , expectNoPath "calling-invalid-2" "simple2_call2" [ ("foo", 12, ViaCall "bar")
                                                     , ("bar", 4, NotViaCall)
                                                     ]
    -- return from the call, even if we started in the callee
  , staticPathTest "returning" "simple2_call" [ ("bar", 2, NotViaCall)
                                              , ("foo", 12, NotViaCall)
                                              ]
    -- can't return to any random place in the caller
  , expectNoPath "returning-invalid" "simple2_call" [ ("bar", 2, NotViaCall)
                                                    , ("foo", 17, NotViaCall)
                                                    ]
  ]

moreblockCallTests :: BenchTest
moreblockCallTests = benchTestGroup "more blocks, calls" [
    -- don't enter the call
    staticPathTest "avoiding-a-call" "simple2_call" $
      bbSequence [3, 8, 12, 17, 8, 12, 17, 8, 20]
    -- can't take the call when we said not to
  , expectNoPath "avoiding-a-call-invalid" "simple2_call" $
      bbSequence [3, 8, 12] ++ [ ("bar", 2, NotViaCall) ]
    -- enter the call
  , staticPathTest "taking-a-call" "simple2_call" $
      [ ("foo", 8, NotViaCall)
      , ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 20]
    -- can't not take the call when we said to
  , expectNoPath "taking-a-call-invalid" "simple2_call" $
      [ ("foo", 8, NotViaCall)
      , ("foo", 12, ViaCall "bar")
      , ("foo", 17, NotViaCall)
      ]
    -- can't return to the wrong place in the caller
  , staticPathTest "taking-then-avoiding" "simple2_call" $
      [ ("foo", 3, NotViaCall)
      , ("foo", 8, NotViaCall)
      , ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 12, 17, 8, 12, 17, 8, 20]
  , staticPathTest "avoiding-then-taking" "simple2_call" $
      bbSequence [3, 8, 12, 17, 8] ++
      [ ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      ] ++
      bbSequence [12, 17, 8]
  , staticPathTest "taking-call-twice" "simple2_call" $
      bbSequence [3, 8, 12, 17, 8] ++
      [ ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 12, 17, 8] ++
      [ ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 20]
  , staticPathTest "avoiding-nested-call" "simple2_call2" $
      bbSequence [3, 8, 12, 17, 8] ++
      [ ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      , ("bar", 6, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 20]
  , staticPathTest "taking-nested-call" "simple2_call2" $
      bbSequence [3, 8, 12, 17, 8] ++
      [ ("foo", 12, ViaCall "bar")
      , ("bar", 2, NotViaCall)
      , ("bar", 6, ViaCall "baz")
      , ("baz", 2, NotViaCall)
      , ("bar", 6, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 20]
  , staticPathTest "returning-from-nested-call" "simple2_call2" $
      [ ("baz", 2, NotViaCall)
      , ("bar", 6, NotViaCall)
      ] ++
      bbSequence [12, 17, 8, 12, 17, 8, 20]
  ]
  where bbSequence = map (\n -> ("foo", n, NotViaCall))

multipleCallsInBlockTests :: BenchTest
multipleCallsInBlockTests = benchTestGroup "multiple calls in block" [
    staticPathTest "ignoring-all-calls-in-block" "multiCallsInBlock" $
      [ ("diffCalls", 2, NotViaCall)
      , ("diffCalls", 9, NotViaCall)
      , ("diffCalls", 23, NotViaCall)
      ]
    -- can't take a call when we said not to
  , expectNoPath "ignoring-all-calls-invalid" "multiCallsInBlock" $
      [ ("diffCalls", 2, NotViaCall)
      , ("diffCalls", 9, NotViaCall)
      , ("callee1", 1, NotViaCall)
      ]
  , staticPathTest "taking-all-calls-in-block" "multiCallsInBlock" $
      [ ("diffCalls", 2, NotViaCall)
      , ("diffCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("diffCalls", 9, ViaCall "callee2")
      , ("callee2", 1, NotViaCall)
      , ("diffCalls", 9, ViaCall "callee3")
      , ("callee3", 1, NotViaCall)
      , ("diffCalls", 9, NotViaCall)
      , ("diffCalls", 23, NotViaCall)
      ]
  , staticPathTest "taking-1st-call-in-block" "multiCallsInBlock" $
      [ ("diffCalls", 2, NotViaCall)
      , ("diffCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("diffCalls", 9, NotViaCall)
      , ("diffCalls", 23, NotViaCall)
      ]
  , staticPathTest "taking-last-call-in-block" "multiCallsInBlock" $
      [ ("diffCalls", 2, NotViaCall)
      , ("diffCalls", 9, ViaCall "callee3")
      , ("callee3", 1, NotViaCall)
      , ("diffCalls", 9, NotViaCall)
      , ("diffCalls", 23, NotViaCall)
      ]
  , staticPathTest "taking-middle-call-in-block" "multiCallsInBlock" $
      [ ("diffCalls", 2, NotViaCall)
      , ("diffCalls", 9, ViaCall "callee2")
      , ("callee2", 1, NotViaCall)
      , ("diffCalls", 9, NotViaCall)
      , ("diffCalls", 23, NotViaCall)
      ]
  , staticPathTest "ignoring-all-id-calls-in-block" "multiCallsInBlock" $
      [ ("sameCalls", 2, NotViaCall)
      , ("sameCalls", 9, NotViaCall)
      , ("sameCalls", 33, NotViaCall)
      ]
    -- can't take a call when we said not to
  , expectNoPath "ignoring-all-id-calls-invalid" "multiCallsInBlock" $
      [ ("sameCalls", 2, NotViaCall)
      , ("sameCalls", 9, NotViaCall)
      , ("callee1", 1, NotViaCall)
      ]
  , staticPathTest "taking-all-id-calls-in-block" "multiCallsInBlock" $
      [ ("sameCalls", 2, NotViaCall)
      , ("sameCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("sameCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("sameCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("sameCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("sameCalls", 9, NotViaCall)
      , ("sameCalls", 33, NotViaCall)
      ]
  , staticPathTest "taking-just-1st-id-call-in-block" "multiCallsInBlock" $
      [ ("sameCalls", 2, NotViaCall)
      , ("sameCalls", 9, ViaCall "callee1")
      , ("callee1", 1, NotViaCall)
      , ("sameCalls", 9, NotViaCall)
      , ("sameCalls", 33, NotViaCall)
      ]
  --, expectNoPath "error-when-id-calls-in-block" "multiCallsInBlock" $
      -- actually, in some cases (as demonstrated in the above tests) we can
      -- handle multiple consecutive calls to the same function in the same
      -- block just fine. The problem is if there are such calls, and you want
      -- to enter the *2nd or later* of them, but *not the 1st*. Or,
      -- generalizing to N identical consecutive calls in a block, the problem
      -- is when you want to enter the kth call but not the jth, for some j < k.
      -- Unfortunately I'm not sure how to even express the desire to enter the
      -- kth consecutive call and not the jth in our current StaticPath data
      -- structure, so I'm not sure how to write a test for this that expects to
      -- fail.
  ]

multipleCallersTests :: BenchTest
multipleCallersTests = benchTestGroup "multiple callers" [
    staticPathTest "call-and-return-caller1" "multipleCallers" $
      [ ("caller1", 6, ViaCall "foo")
      ] ++
      bbSequence [3, 15, 19] ++
      [ ("caller1", 6, NotViaCall)
      , ("caller1", 16, NotViaCall)
      ]
  , staticPathTest "call-and-return-caller2" "multipleCallers" $
      [ ("caller2", 13, ViaCall "foo")
      ] ++
      bbSequence [3, 11, 19] ++
      [ ("caller2", 13, NotViaCall)
      , ("caller2", 20, NotViaCall)
      ]
    -- can't return to the wrong caller
  , expectNoPath "call-and-return-invalid" "multipleCallers" $
      [ ("caller2", 13, ViaCall "foo")
      ] ++
      bbSequence [3, 11, 19] ++
      [ ("caller1", 6, NotViaCall)
      ]
    -- can't return to the wrong callsite in the right caller
  , expectNoPath "call-and-return-invalid-2" "multipleCallers" $
      [ ("caller2", 6, ViaCall "foo")
      ] ++
      bbSequence [3, 11, 19] ++
      [ ("caller2", 13, NotViaCall)
      ]
  , staticPathTest "return-to-caller1-6" "multipleCallers" $
      [ ("foo", 19, NotViaCall), ("caller1", 6, NotViaCall) ]
  , staticPathTest "return-to-caller1-11" "multipleCallers" $
      [ ("foo", 19, NotViaCall), ("caller1", 11, NotViaCall) ]
  , staticPathTest "return-to-caller2-6" "multipleCallers" $
      [ ("foo", 19, NotViaCall), ("caller2", 6, NotViaCall) ]
  , staticPathTest "return-to-caller2-13" "multipleCallers" $
      [ ("foo", 19, NotViaCall), ("caller2", 13, NotViaCall) ]
  , staticPathTest "call-and-return-nested" "multipleCallers" $
      [ ("callercaller", 2, ViaCall "caller1")
      , ("caller1", 1, NotViaCall)
      , ("caller1", 6, ViaCall "foo")
      ] ++
      bbSequence [3, 15, 19] ++
      [ ("caller1", 6, NotViaCall)
      , ("caller1", 16, NotViaCall)
      , ("callercaller", 2, NotViaCall)
      , ("callercaller", 9, ViaCall "caller2")
      , ("caller2", 1, NotViaCall)
      , ("caller2", 13, ViaCall "foo")
      ] ++
      bbSequence [3, 11, 19] ++
      [ ("caller2", 13, NotViaCall)
      , ("caller2", 20, NotViaCall)
      , ("callercaller", 9, NotViaCall)
      , ("callercaller", 17, NotViaCall)
      ]
  , staticPathTest "return-nested-via-caller1-6" "multipleCallers" $
      [ ("foo", 19, NotViaCall)
      , ("caller1", 6, NotViaCall)
      , ("caller1", 16, NotViaCall)
      , ("callercaller", 2, NotViaCall)
      ]
  , staticPathTest "return-nested-via-caller1-11" "multipleCallers" $
      [ ("foo", 19, NotViaCall)
      , ("caller1", 11, NotViaCall)
      , ("caller1", 16, NotViaCall)
      , ("callercaller", 2, NotViaCall)
      ]
  , staticPathTest "return-nested-via-caller2-6-to-9" "multipleCallers" $
      [ ("foo", 19, NotViaCall)
      , ("caller2", 6, NotViaCall)
      , ("caller2", 20, NotViaCall)
      , ("callercaller", 9, NotViaCall)
      ]
  , staticPathTest "return-nested-via-caller2-6-to-13" "multipleCallers" $
      [ ("foo", 19, NotViaCall)
      , ("caller2", 6, NotViaCall)
      , ("caller2", 20, NotViaCall)
      , ("callercaller", 13, NotViaCall)
      ]
  , staticPathTest "return-nested-via-caller2-13-to-9" "multipleCallers" $
      [ ("foo", 19, NotViaCall)
      , ("caller2", 13, NotViaCall)
      , ("caller2", 20, NotViaCall)
      , ("callercaller", 9, NotViaCall)
      ]
  , staticPathTest "return-nested-via-caller2-13-to-13" "multipleCallers" $
      [ ("foo", 19, NotViaCall)
      , ("caller2", 13, NotViaCall)
      , ("caller2", 20, NotViaCall)
      , ("callercaller", 13, NotViaCall)
      ]
  ]
  where bbSequence = map (\n -> ("foo", n, NotViaCall))

recursiveTests :: BenchTest
recursiveTests = benchTestGroup "recursive" [
    staticPathTest "avoiding-recursive-call" "recursiveCalls" $
      bbSequenceSimple [1, 11, 14]
  , staticPathTest "taking-recursive-call" "recursiveCalls" $
      [ ("simple", 1, NotViaCall)
      , ("simple", 11, ViaCall "simple")
      , ("simple", 1, NotViaCall)
      , ("simple", 11, ViaCall "simple")
      ] ++ bbSequenceSimple [1, 9, 14, 11, 14, 11, 14]
  , staticPathTest "returning-recursive-calls" "recursiveCalls" $
      bbSequenceSimple [1, 9, 14, 11, 14, 11, 14]  -- starting with recursive calls already in progress
  , staticPathTest "more-blocks-recursive" "recursiveCalls" $
      bbSequenceMore [1, 13, 17, 19, 20, 21, 24, 27, 21, 30] ++
      [ ("moreBlocks", 39, ViaCall "moreBlocks") ] ++
      bbSequenceMore [1, 13, 17, 19, 20, 21, 24, 27, 21, 30, 35, 42, 39, 42]
  , staticPathTest "not-tail-recursive" "recursiveCalls" $
      [ ("notTailRec", 1, NotViaCall)
      , ("notTailRec", 10, ViaCall "notTailRec")
      , ("notTailRec", 1, NotViaCall)
      , ("notTailRec", 10, ViaCall "notTailRec")
      ] ++
      bbSequenceNotTail [1, 7, 22, 10, 16, 22, 10, 19, 22]
  , staticPathTest "mutually-recursive" "recursiveCalls" $
      [ ("mutRecursiveA", 1, NotViaCall)
      , ("mutRecursiveA", 8, ViaCall "mutRecursiveB")
      , ("mutRecursiveB", 1, NotViaCall)
      , ("mutRecursiveB", 8, ViaCall "mutRecursiveA")
      , ("mutRecursiveA", 1, NotViaCall)
      , ("mutRecursiveA", 8, ViaCall "mutRecursiveB")
      , ("mutRecursiveB", 1, NotViaCall)
      , ("mutRecursiveB", 6, NotViaCall)
      , ("mutRecursiveB", 12, NotViaCall)  -- return
      , ("mutRecursiveA", 8, NotViaCall)
      , ("mutRecursiveA", 12, NotViaCall)  -- return
      , ("mutRecursiveB", 8, NotViaCall)
      , ("mutRecursiveB", 12, NotViaCall)  -- return
      , ("mutRecursiveA", 8, NotViaCall)
      , ("mutRecursiveA", 12, NotViaCall)  -- return
      ]
  , staticPathTest "returning-mut-recursive" "recursiveCalls" $
      [ ("mutRecursiveB", 1, NotViaCall)
      , ("mutRecursiveB", 6, NotViaCall)
      , ("mutRecursiveB", 12, NotViaCall)
      , ("mutRecursiveA", 8, NotViaCall)
      , ("mutRecursiveA", 12, NotViaCall)
      , ("mutRecursiveB", 8, NotViaCall)
      , ("mutRecursiveB", 12, NotViaCall)
      , ("mutRecursiveA", 8, NotViaCall)
      , ("mutRecursiveA", 12, NotViaCall)
      ]
  , staticPathTest "3-way-mut-recursive" "recursiveCalls" $
      [ ("threeWayMutRecursiveA", 1, NotViaCall)
      , ("threeWayMutRecursiveA", 9, ViaCall "threeWayMutRecursiveB")
      , ("threeWayMutRecursiveB", 1, NotViaCall)
      , ("threeWayMutRecursiveB", 9, ViaCall "threeWayMutRecursiveC")
      , ("threeWayMutRecursiveC", 1, ViaCall "threeWayMutRecursiveA")
      , ("threeWayMutRecursiveA", 1, NotViaCall)
      , ("threeWayMutRecursiveA", 9, ViaCall "threeWayMutRecursiveB")
      , ("threeWayMutRecursiveB", 1, NotViaCall)
      , ("threeWayMutRecursiveB", 6, NotViaCall)
      , ("threeWayMutRecursiveB", 13, NotViaCall)  -- return
      , ("threeWayMutRecursiveA", 9, NotViaCall)
      , ("threeWayMutRecursiveA", 13, NotViaCall)  -- return
      , ("threeWayMutRecursiveC", 1, NotViaCall)   -- return
      , ("threeWayMutRecursiveB", 9, NotViaCall)
      , ("threeWayMutRecursiveB", 13, NotViaCall)  -- return
      , ("threeWayMutRecursiveA", 9, NotViaCall)
      , ("threeWayMutRecursiveA", 13, NotViaCall)  -- return
      ]
  , staticPathTest "nested-recursive" "recursiveCalls" $
      [ ("nestedRecursive", 1, ViaCall "notRecursive")
      , ("notRecursive", 2, NotViaCall)      -- return
      , ("nestedRecursive", 1, NotViaCall)
      , ("nestedRecursive", 11, ViaCall "nestedRecursive")
      , ("nestedRecursive", 1, ViaCall "notRecursive")
      , ("notRecursive", 2, NotViaCall)      -- return
      , ("nestedRecursive", 1, NotViaCall)
      , ("nestedRecursive", 9, NotViaCall)
      , ("nestedRecursive", 14, NotViaCall)  -- return
      , ("nestedRecursive", 11, NotViaCall)
      , ("nestedRecursive", 14, NotViaCall)  -- return
      ]
  , staticPathTest "double-recursive" "recursiveCalls" $
      [ ("doubleRecursive", 2, NotViaCall)
      , ("doubleRecursive", 16, ViaCall "doubleRecursive")  -- call 1st entry in block 16
      , ("doubleRecursive", 2, NotViaCall)
      , ("doubleRecursive", 16, ViaCall "doubleRecursive")  -- call 1st entry in block 16
      , ("doubleRecursive", 2, NotViaCall)
      , ("doubleRecursive", 12, NotViaCall)
      , ("doubleRecursive", 29, NotViaCall)  -- return
      , ("doubleRecursive", 16, ViaCall "doubleRecursive")  -- call 2nd entry in block 16
      , ("doubleRecursive", 2, NotViaCall)
      , ("doubleRecursive", 12, NotViaCall)
      , ("doubleRecursive", 29, NotViaCall)  -- return
      , ("doubleRecursive", 16, NotViaCall)  -- finish block, done with both calls
      , ("doubleRecursive", 29, NotViaCall)  -- return
      , ("doubleRecursive", 16, NotViaCall)  -- skip over 2nd entry in block 16 (testing entering the 1st and not the 2nd)
      , ("doubleRecursive", 29, NotViaCall)  -- return
      ]
  ]
  where bbSequenceSimple = map (\n -> ("simple", n, NotViaCall))
        bbSequenceMore = map (\n -> ("moreBlocks", n, NotViaCall))
        bbSequenceNotTail = map (\n -> ("notTailRec", n, NotViaCall))

crossModuleTests :: BenchTest
crossModuleTests = benchTestGroup "cross-module" [
    staticPathTestMultiMod "cross-mod-call-from-simple" ["simple1-O2", "crossModule"] $
      [ (1, "simpleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 12] ++
      [ (1, "simpleCrossModuleCaller", 2, NotViaCall) ]
  , staticPathTestMultiMod "cross-mod-call-from-multiple" ["simple1-O2", "crossModule"] $
      [ (1, "multipleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 5, 7, 12] ++
      [ (1, "multipleCrossModuleCaller", 2, NotViaCall)
      , (1, "multipleCrossModuleCaller", 24, ViaCall "foo")
      ] ++
      bbSequence [3, 5, 7, 12] ++
      [ (1, "multipleCrossModuleCaller", 24, NotViaCall)
      , (1, "multipleCrossModuleCaller", 29, ViaCall "foo")
      ] ++
      bbSequence [3, 12] ++
      [ (1, "multipleCrossModuleCaller", 29, NotViaCall)
      , (1, "multipleCrossModuleCaller", 49, NotViaCall)
      ]
    -- can't return to the wrong caller
  , expectNoPathMultiMod "cross-mod-call-invalid" ["simple1-O2", "crossModule"] $
      [ (1, "multipleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 5, 7, 12] ++
      [ (1, "simpleCrossModuleCaller", 2, NotViaCall) ]
    -- can't return to the wrong callsite in the right caller
  , expectNoPathMultiMod "cross-mod-call-invalid-2" ["simple1-O2", "crossModule"] $
      [ (1, "multipleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 5, 7, 12] ++
      [ (1, "multipleCrossModuleCaller", 24, NotViaCall) ]
    -- cross-module call works even when there are other callers in both modules
  , staticPathTestMultiMod "cross-mod-call-to-multiple" ["multipleCallers", "crossModule"] $
      [ (1, "multipleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 11, 19] ++
      [ (1, "multipleCrossModuleCaller", 2, NotViaCall) ]
    -- can't return to caller in your own module if you were called from a different module
  , expectNoPathMultiMod "cross-mod-call-invalid-3" ["multipleCallers", "crossModule"] $
      [ (1, "multipleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 11, 19] ++
      [ (0, "caller1", 11, NotViaCall) ]
    -- in the current implementation, this extra modules won't even make it into the StaticPath, so the test is kinda useless as written
  , staticPathTestMultiMod "extraneous-modules" ["simple1-O2", "recursiveCalls", "multiCallsInBlock", "crossModule"] $
      [ (3, "simpleCrossModuleCaller", 2, ViaCall "foo") ] ++
      bbSequence [3, 12] ++
      [ (3, "simpleCrossModuleCaller", 2, NotViaCall) ]
  , staticPathTestMultiMod "triple-module" ["simple1-O2", "crossModule", "crossModule2"] $
      [ (2, "simple", 2, ViaCall "simpleCrossModuleCaller")
      , (1, "simpleCrossModuleCaller", 2, ViaCall "foo")
      ] ++
      bbSequence [3, 12] ++
      [ (1, "simpleCrossModuleCaller", 2, NotViaCall)
      , (2, "simple", 2, NotViaCall)
      ]
  , staticPathTestMultiMod "triple-module-to-multiple" ["multipleCallers", "crossModule", "crossModule2"] $
      [ (2, "simple", 2, ViaCall "simpleCrossModuleCaller")
      , (1, "simpleCrossModuleCaller", 2, ViaCall "foo")
      ] ++
      bbSequence [3, 15, 19] ++
      [ (1, "simpleCrossModuleCaller", 2, NotViaCall)
      , (2, "simple", 2, NotViaCall)
      ]
  , staticPathTestMultiMod "return-cross-mod" ["simple1-O2", "crossModule"] $
      bbSequence [3, 5, 7, 12] ++
      [ (1, "simpleCrossModuleCaller", 2, NotViaCall) ]
  , staticPathTestMultiMod "ignore-caller-in-same-module" ["multipleCallers", "crossModule"] $
      bbSequence [3, 15, 19] ++
      [ (1, "multipleCrossModuleCaller", 29, NotViaCall) ]
  , staticPathTestMultiMod "ignore-caller-in-other-module" ["multipleCallers", "crossModule"] $
      bbSequence [15, 19] ++
      [ (0, "caller1", 11, NotViaCall) ]
  , staticPathTestMultiMod "return-cross-mod-triple-mod" ["multipleCallers", "crossModule", "crossModule2"] $
      bbSequence [3, 11, 19] ++
      [ (1, "simpleCrossModuleCaller", 2, NotViaCall)
      , (2, "simple", 2, NotViaCall)
      ]
  ]
  where bbSequence = map (\n -> (0, "foo", n, NotViaCall))

llvmPath :: String
llvmPath = "test/Pathsearch/llvm/"

-- | Test staticToSimple on the given static path and ensure the result matches golden file
staticPathTest :: String   -- ^ Test name (also determines golden file name)
               -> FilePath -- ^ LLVM file
               -> [(String, Word, StaticPathEntryTerminator)] -- ^ (Function name, BB number, term) triples for making the StaticPath
               -> BenchTest
staticPathTest testName filename triples =
  staticPathTestMultiMod testName [filename] $ map (\(a,b,t) -> (0,a,b,t)) triples

-- | Like staticPathTest, but we expect that the static path should not produce a simple path.
expectNoPath :: String   -- ^ Test name
             -> FilePath -- ^ LLVM file
             -> [(String, Word, StaticPathEntryTerminator)] -- ^ (Function name, BB number, term) triples for making the StaticPath
             -> BenchTest
expectNoPath testName filename triples =
  expectNoPathMultiMod testName [filename] $ map (\(a,b,t) -> (0,a,b,t)) triples

-- | staticPathTest, generalized to cross-module paths. See notes on staticPathTest
staticPathTestMultiMod :: String     -- ^ Test name (also determines golden file name)
                       -> [FilePath] -- ^ LLVM files touched by the static path
                       -> [(Int, String, Word, StaticPathEntryTerminator)]
                          -- ^ (Module num, function name, BB number, term)
                          -- where "module num" is just the index in the list of files given above.
                          -- So "0" refers to the first file in the list.
                       -> BenchTest
staticPathTestMultiMod testName filenames tuples = benchTestCase testName $
  withContext $ \ctx -> do
    mods <- mapM (getModuleInfoFromFile ctx) $ map (\f -> llvmPath ++ f ++ ".ll") filenames
    let tuples' = map (\(m, a, b, t) -> (mods !! m, mkName a, UnName b, t)) tuples
        statpath = mkStatPath tuples'
    msimppath <- staticToSimple statpath
    case msimppath of
      Left s -> assertFailure $ "expected to get a SimplePath, got error: " ++ s
      Right simppath -> do
        let goldenFile = "test/Pathsearch/paths/from_static/" ++ testName
        generateGoldenFiles <- isJust <$> lookupEnv "GENERATE"  -- if the environment variable "GENERATE" is defined at all, then we are generating golden files
        when generateGoldenFiles $ T.writeFile goldenFile $ showGolden simppath
        goldenPath <- T.readFile goldenFile
        showGolden simppath @?= goldenPath

-- | expectNoPath, generalized to cross-module paths. See notes on expectNoPath
expectNoPathMultiMod :: String     -- ^ Test name
                     -> [FilePath] -- ^ LLVM files touched by the static path
                     -> [(Int, String, Word, StaticPathEntryTerminator)]
                        -- ^ (Module num, function name, BB number, term)
                        -- where "module num" is just the index in the list of files given above.
                        -- So "0" refers to the first file in the list.
                     -> BenchTest
expectNoPathMultiMod testName filenames tuples = benchTestCase testName $
  withContext $ \ctx -> do
    mods <- mapM (getModuleInfoFromFile ctx) $ map (\f -> llvmPath ++ f ++ ".ll") filenames
    let tuples' = map (\(m, a, b, t) -> (mods !! m, mkName a, UnName b, t)) tuples
        statpath = mkStatPath tuples'
    msimppath <- staticToSimple statpath
    case msimppath of
      Left _  -> return ()
      Right _ -> assertFailure $ "expected to not get a SimplePath, but got one"

-- Alternate Show instance for SimplePath meant to produce the same format as our golden files.
-- Stolen from test/Pathsearch.hs, see notes there
showGolden :: SimplePath -> T.Text
showGolden (SimplePath is _) = T.intercalate "\n" (map (T.pack . show) is) `T.append` "\n"

-- | Grab valid basic block sequences from SimplePaths and turn them into StaticPaths.
-- Then we'll make sure that these StaticPaths convert back into the right SimplePaths.
-- This function currently only works for SimplePaths which don't enter any calls.
_statFromSimple :: SimplePath -> StaticPath
_statFromSimple simppath | any isCallMarker (instrs simppath) = error "statFromSimple currently doesn't support SimplePaths which enter calls"
_statFromSimple simppath = map (\spbe -> mkSPE (spbeMod spbe) (spbeFunc spbe) (spbeBB spbe)) $ bbs simppath
    where mkSPE :: ModuleInfo -> Name -> BasicBlock -> StaticPathEntry
          mkSPE modInfo fName bb = StaticPathEntry { speModule = modInfo
                                                   , speFunc = fName
                                                   , speBasicBlock = bb
                                                   , speTerminator = NotViaCall
                                                   }

mkStatPath :: [(ModuleInfo, Name, Name, StaticPathEntryTerminator)] -- ^ (Module, function, BB, term) tuples
           -> StaticPath
mkStatPath = map mkSPE
  where mkSPE :: (ModuleInfo, Name, Name, StaticPathEntryTerminator) -> StaticPathEntry
        mkSPE (modInfo, funcName, bbName, term) = StaticPathEntry {
            speModule     = modInfo
          , speFunc       = funcName
          , speBasicBlock = bbFromName modInfo funcName bbName
          , speTerminator = term
          }

bbFromName :: ModuleInfo -- ^ Module
           -> Name -- ^ Function name; must be a function in the Module
           -> Name -- ^ BB name; must be a BB in the function
           -> BasicBlock
bbFromName modInfo funcName bbName =
  let funcbbs = fromJust $ bbsFromFunctionName (modAST modInfo) funcName
  in  case filter (\bb -> getBlockName bb == bbName) funcbbs of
        [bb] -> bb
        []   -> error $ "bbFromName: couldn't find a bb named " ++ show bbName
        _    -> error $ "bbFromName: found multiple bbs named " ++ show bbName
