{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Types where
import qualified Data.Map as M
import           Utils

typeTests :: BenchTest
typeTests = benchTestGroup "Type tests"
            [ benchTestGroup "Simple types" [ integer
                                            , pointer
                                            , uninitPointer
                                            , namedReference
                                            ]
            , benchTestGroup "Aggregate types" [ vector
                                               , array
                                               , dynamicarray1
                                               , struct
                                               , pstruct
                                               ]
            ]

typePath :: String
typePath = "test/Backend/Types/"

integer :: BenchTest
integer = let expectedVars = M.fromList [ ("add_i1_0", 1)
                                        , ("add_i1_1", 0)
                                        , ("add_i2_0", 1)
                                        , ("add_i2_1", 2)
                                        ]
          in variableAssignmentTest expectedVars $ typePath ++ "integers.ll"

pointer :: BenchTest
pointer = let expectedVars = M.fromList [ ("add_result_0", 1)
                                        , ("add_val_2", 100)
                                        , ("add_vect_one", 200)
                                        , ("add_result_3", 4)
                                        , ("add_sval", 200)
                                        , ("add_innerval", 8)
                                        , ("add_v", 100)
                                        , ("add_d", 200)
                                        , ("add_nullval", 0)
                                        ]
          in variableAssignmentTestAlias expectedVars $ typePath ++ "pointer.ll"

uninitPointer :: BenchTest
uninitPointer = let expectedVars = M.fromList [ ("add_alias", 0)
                                              , ("add_alias1", 0)
                                              , ("add_alias2", 0)
                                              , ("add_alias3", 0)
                                              , ("add_alias4", 0)
                                              , ("add_alias5", 0)
                                              ]
                in variableAssignmentTest expectedVars $ typePath ++ "uninitPointer.ll"

namedReference :: BenchTest
namedReference = let expectedVars = M.fromList [ ("add_aval", 5)
                                               , ("add_bresult_0", 5)
                                               , ("add_bresult_1", 500)
                                               , ("add_y", 100)
                                               , ("add_d_r0", 0)
                                               , ("add_d_r1", 1)
                                               , ("add_v", 1)
                                               ]
                 in variableAssignmentTest expectedVars $ typePath ++ "namedReference.ll"

vector :: BenchTest
vector = let expectedVars = M.fromList [ ("add_z", 6)
                                       , ("add_c", 17)
                                       ]
         in variableAssignmentTest expectedVars $ typePath ++ "vector.ll"

array :: BenchTest
array = let expectedVars = M.fromList [ ("simple_letter", 122) -- z
                                      , ("simple_term", 0)
                                      , ("simple_letter1", 97) -- a
                                      ,  ("simple_letter2", 100) -- d
                                      , ("simple_letter3", 100)
                                      , ("simple_letter4", 50)
                                      , ("simple_letter5", 100)
                                      , ("simple_letter6", 50)
                                      , ("simple_letter7", 9)
                                      , ("simple_letter8", 11)
                                      ]
        in variableAssignmentTest expectedVars $ typePath ++ "array.ll"

dynamicarray1 :: BenchTest
dynamicarray1 = let expectedVars = M.fromList [ ("dynamicarr_r0", 2)
                                              , ("dynamicarr_r1", 50)
                                              , ("dynamicarr_r2", 4)
                                              , ("dynamicarr_r3", 35)
                                              , ("dynamicarr_r4", 9)
                                              , ("dynamicarr_r5", 18)
                                              , ("dynamicarr_r6", 53)
                                              , ("dynamicarr_r7", 7)
                                              , ("dynamicarr_rs1", 30)
                                              , ("dynamicarr_rs2", 48)
                                              , ("dynamicarr_rs3", 99)
                                              , ("dynamicarr_rs4", 4)
                                              , ("dynamicarr_rt0", 2)
                                              , ("dynamicarr_rt1", 4)
                                              , ("dynamicarr_reight", 255)
                                              , ("dynamicarr_rsixteen", 65535)
                                              , ("dynamicarr_rthirtytwo", 4294967295)
                                              ]
                in variableAssignmentTest expectedVars $ typePath ++ "dynamicarray.ll"

struct :: BenchTest
struct = let expectedVars = M.fromList [ ("add_result", 1000)
                                       , ("add_ptr1", 0)
                                       ]
         in variableAssignmentTestAlias expectedVars $ typePath ++ "struct.ll"

pstruct :: BenchTest
pstruct = let expectedVars = M.fromList [ ("add_v", 45)
                                        , ("add_w", 127)
                                        , ("add_b", 12345)
                                        ]
          in variableAssignmentTestAlias expectedVars $ typePath ++ "packedStruct.ll"
