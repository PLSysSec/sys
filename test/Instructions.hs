{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Instructions where
import qualified Data.Map as M
import           Utils

instructionsFilePath :: String
instructionsFilePath = "test/Backend/Instructions/"

instructionTests :: BenchTest
instructionTests =
   benchTestGroup
      "Instructions"
      [ benchTestGroup
          "BinOps"
          [ addO3
          , subO3
          , mulO3
          , udivO3
          , sdivO3
          , uremO3
          , sremO3
          , shlO3
          , lshrO3
          , ashrO3
          , andO3
          , orO3
          , xorO3
          ]
      , benchTestGroup
            "Vectors"
        [ insertextractelementO3
        , shufflevectorO3
        ]
      , benchTestGroup "Aggregates" [ insertextractvalueO3 ]
     , benchTestGroup "Memory"
           [ loadO3
           , cmpxchgO3
           , atomicrmwO3
           , getelementptrO3
           ]
     , benchTestGroup
            "Casts"
            [ truncO3
            , zextO3
            , sextO3
            , ptrtointO3
            , inttoptrO3
            , bitcastO3
            ]
     , benchTestGroup "Other" [icmpO3, selectO3]
    ]

addO3 :: BenchTest
addO3 = let expectedVars = M.fromList [ ("add_3", 2)
                                      , ("add_4", 0) ]
        in variableAssignmentTest expectedVars $ instructionsFilePath ++ "add_O3.ll"

subO3 :: BenchTest
subO3 = let expectedVars = M.fromList [("sub_3", 4294967295)]
        in variableAssignmentTest expectedVars $ instructionsFilePath ++ "sub_O3.ll"

mulO3 :: BenchTest
mulO3 = let expectedVars = M.fromList [ ("mul_3", 254)
                                      , ("mul_4", 0)
                                      , ("mul_5", 0) ]
        in variableAssignmentTest expectedVars $ instructionsFilePath ++ "mul_O3.ll"

udivO3 :: BenchTest
udivO3 = let expectedVars = M.fromList [ ("udiv_3", 0)
                                       , ("udiv_4", 127)
                                       ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "udiv_O3.ll"

sdivO3 :: BenchTest
sdivO3 = let expectedVars = M.fromList [ ("sdiv_3", 255)
                                       , ("sdiv_4", 0)
                                       , ("sdiv_5", 62) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "sdiv_O3.ll"

uremO3 :: BenchTest
uremO3 = let expectedVars = M.fromList [ ("urem_3", 1)
                                       , ("urem_4", 0)
                                       , ("urem_5", 3) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "urem_O3.ll"

sremO3 :: BenchTest
sremO3 = let expectedVars = M.fromList [ ("srem_3", 0)
                                       , ("srem_4", 255)
                                       , ("srem_5", 1) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "srem_O3.ll"

shlO3 :: BenchTest
shlO3 = let expectedVars = M.fromList [ ("shl_3", 16)
                                      , ("shl_4", 240)
                                      , ("shl_5", 0) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "shl_O3.ll"

lshrO3 :: BenchTest
lshrO3 = let expectedVars = M.fromList [ ("lshr_3", 0)
                                       , ("lshr_4", 15) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "lshr_O3.ll"

ashrO3 :: BenchTest
ashrO3 = let expectedVars = M.fromList [ ("ashr_3", 255)
                                       , ("ashr_4", 7)
                                       , ("ashr_5", 255)
                                       , ("ashr_6", 0) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "ashr_O3.ll"

andO3 :: BenchTest
andO3 = let expectedVars = M.fromList [ ("and_3", 0)
                                      , ("and_4", 255)
                                      , ("and_5", 4) ]
        in variableAssignmentTest expectedVars $ instructionsFilePath ++ "and_O3.ll"

orO3 :: BenchTest
orO3 = let expectedVars = M.fromList [ ("or_3", 255)
                                     , ("or_4", 255)
                                     , ("or_5", 0)
                                     ]
       in variableAssignmentTest expectedVars $ instructionsFilePath ++ "or_O3.ll"

xorO3 :: BenchTest
xorO3 = let expectedVars = M.fromList [ ("xor_3", 255)
                                      , ("xor_4", 0)
                                      , ("xor_5", 0)
                                      ]
        in variableAssignmentTest expectedVars $ instructionsFilePath ++ "xor_O3.ll"

-- Vector operations

insertextractelementO3 :: BenchTest
insertextractelementO3 = let expectedVars = M.fromList [ ("or_4", 2)
                                                       , ("or_6", 2)
                                                       , ("or_7", 4)
                                                       ]
                         in variableAssignmentTest expectedVars $
                            instructionsFilePath ++ "extractelement_O3.ll"

shufflevectorO3 :: BenchTest
shufflevectorO3 = let expectedVars = M.fromList [ ("or_zero", 0)
                                                , ("or_three", 3)
                                                , ("or_onehundred", 100)
                                                ]
                  in variableAssignmentTest expectedVars $
                     instructionsFilePath ++ "shufflevector_O3.ll"

-- Aggregate instructions

extractvalueO3 :: BenchTest
extractvalueO3 = let expectedVars = M.fromList [("or_3", 0)]
                 in variableAssignmentTest expectedVars $
                    instructionsFilePath ++ "extractvalue_O3.ll"

insertextractvalueO3 :: BenchTest
insertextractvalueO3 = let expectedVars = M.fromList [ ("simple_result", 200)
                                                     , ("simple_extract", 400)
                                                     , ("simple_load", 0)
                                                     ]
                       in variableAssignmentTest expectedVars $
                          instructionsFilePath ++ "insertvalue_O3.ll"

-- Memory operations

-- This is commented out because it applied when we did our own sizing.
-- Now, we use LLVM to do sizing
-- allocaO3 :: BenchTest
-- allocaO3 = let expectedVars = M.fromList [ ("alloca_ptr1", 64)
--                                          , ("alloca_ptr2", 128)
--                                          , ("alloca_ptr3", 192)
--                                          , ("alloca_ptr4", 224)
--                                          , ("alloca_ptr5", 240)
--                                          , ("alloca_ptr6", 248)
--                                          , ("alloca_ptr7", 256)
--                                          , ("alloca_ptr8", 320)
--                                          , ("alloca_ptr9", 384)
--                                          ]
--            in variableAssignmentTest expectedVars $ instructionsFilePath ++ "alloca_O3.ll"

loadO3 :: BenchTest
loadO3 = let expectedVars = M.fromList [ ("load_w4", 3) -- ok
                                       , ("load_w8", 4) -- ok
                                       , ("load_w12", 12) -- ok
                                       , ("load_w13", 14) -- ok
                                       , ("load_w17", 7) -- ok
                                       , ("load_w19", 5) -- 0 BAD
                                       , ("load_w20", 8) -- 0 BAD
                                       , ("load_w26", 5) -- ok
                                       , ("load_w27", 5) -- ok
                                       , ("load_a3", 9) --ok
                                       , ("load_a4", 5) -- ok
                                       , ("load_ev1", 9) --ok
                                       ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "load_O3.ll"

cmpxchgO3 :: BenchTest
cmpxchgO3 = let expectedVars = M.fromList [ ("cmpxchg_val0", 2)
                                          , ("cmpxchg_val1", 1)
                                          , ("cmpxchg_val2", 100)
                                          ]
            in variableAssignmentTest expectedVars $ instructionsFilePath ++ "cmpxchg_O3.ll"

atomicrmwO3 :: BenchTest
atomicrmwO3 = let expectedVars = M.fromList [ ("or_4", 0)
                                            , ("or_5", 100)
                                            , ("or_7", 100)
                                            , ("or_8", 200)
                                            , ("or_10", 200)
                                            , ("or_11", 0)
                                            , ("or_13", 0)
                                            , ("or_14", 0)
                                            , ("or_16", 0)
                                            , ("or_17", 18446744073709551615)
                                            , ("or_19", 18446744073709551615)
                                            , ("or_20", 18446744073709551615)
                                            , ("or_22", 18446744073709551615)
                                            , ("or_23", 18446744073709551615)
                                            , ("or_25", 18446744073709551615)
                                            , ("or_26", 1)
                                            , ("or_28", 1)
                                            , ("or_29", 18446744073709551615)
                                            , ("or_31", 18446744073709551615)
                                            , ("or_32", 18446744073709551615)
                                            , ("or_34", 18446744073709551615)
                                            , ("or_35", 1)
                                            ]
              in variableAssignmentTest expectedVars $ instructionsFilePath ++ "atomicrmw_O3.ll"

getelementptrO3 :: BenchTest
getelementptrO3 = let expectedVars = M.fromList [ ("simple_value", 900)
                                                , ("simple_value1", 50)
                                                , ("simple_value2", 200)
                                                , ("simple_value3", 25)
                                                , ("simple_value4", 50)
                                                , ("simple_value5", 8)
                                                , ("simple_value6", 2)
                                                , ("simple_y", 1000)
                                                , ("simple_k", 1000)
                                                , ("simple_val", 4)
                                                , ("simple_arrval", 50)
                                                , ("simple_badl", 18)
                                                , ("simple_badll", 2)
                                                , ("simple_badlll", 9)
                                                , ("simple_badllll", 1)
                                                ]
                  in variableAssignmentTestAlias expectedVars $ instructionsFilePath ++ "getelementptr_O3.ll"

-- Cast

truncO3 :: BenchTest
truncO3 = let expectedVars = M.fromList [ ("trunc_3", 1)
                                        , ("trunc_4", 0)
                                        , ("trunc_5", 0)
                                        , ("trunc_6", 1)
                                        ]
          in variableAssignmentTest expectedVars $ instructionsFilePath ++ "trunc_O3.ll"

zextO3 :: BenchTest
zextO3 = let expectedVars = M.fromList [ ("zext_3", 1)
                                       , ("zext_4", 0)
                                       , ("zext_5", 255) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "zext_O3.ll"

sextO3 :: BenchTest
sextO3 = let expectedVars = M.fromList [ ("sext_3", 255)
                                       , ("sext_4", 0)
                                       , ("sext_5", 4294967295) ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "sext_O3.ll"

ptrtointO3 :: BenchTest
ptrtointO3 = let expectedVars = M.fromList [ ("ptrtoint_3", 1)
                                           , ("ptrtoint_4", 1) ]
             in variableAssignmentTest expectedVars $ instructionsFilePath ++ "ptrtoint_O3.ll"

inttoptrO3 :: BenchTest
inttoptrO3 = let expectedVars = M.fromList [ ("inttoptr_3", 0)
                                           , ("inttoptr_4", 1)
                                           , ("inttoptr_5", 255) ]
             in variableAssignmentTest expectedVars $ instructionsFilePath ++ "inttoptr_O3.ll"

bitcastO3 :: BenchTest
bitcastO3 = let expectedVars = M.fromList [("bitcast_3", 255)]
            in variableAssignmentTest expectedVars $ instructionsFilePath ++ "bitcast_O3.ll"

-- Other

icmpO3 :: BenchTest
icmpO3 = let expectedVars = M.fromList [ ("or_3", 1)
                                       , ("or_4", 0)
                                       , ("or_5", 1)
                                       , ("or_6", 1)
                                       , ("or_7", 0)
                                       , ("or_8", 1)
                                       , ("or_9", 0)
                                       , ("or_10", 1)
                                       , ("or_11", 1)
                                       , ("or_12", 1)
                                       ]
         in variableAssignmentTest expectedVars $ instructionsFilePath ++ "icmp_O3.ll"

selectO3 :: BenchTest
selectO3 = let expectedVars = M.fromList [("or_4", 1)]
           in variableAssignmentTest expectedVars $ instructionsFilePath ++ "select_O3.ll"
