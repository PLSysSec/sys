{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Constants where
import qualified Data.Map as M
import           Prelude  hiding (null)
import           Utils

constantTests :: BenchTest
constantTests = benchTestGroup "Constant tests"
                [ benchTestGroup "Simple constants" [ null
                                                    , aggregateZero
                                                    , struct
                                                    , array
                                                    , vector
                                                    , undef
                                                    , globalVariable
                                                    ]
                 , benchTestGroup "Operation constants" [ binOp
                                                        , getElementPtr
                                                        , castOp
                                                        , cmpOp
                                                        , selectOp
                                                        , insertElement
                                                        , extractElement
                                                        , shuffleVector
                                                        , insertValue
                                                        , extractValue
                                                        ]
                ]

constantPath :: String
constantPath = "test/Backend/Constants/"

-- Simple constants

null :: BenchTest
null = let expectedVars = M.fromList [ ("simple_2", 0)
                                     , ("simple_3", 0)
                                     , ("simple_6", 0)
                                     , ("simple_8", 0)
                                     , ("simple_11", 0)
                                     , ("simple_12", 0)
                                     ]
       in variableAssignmentTestAlias expectedVars $ constantPath ++ "null.ll"

aggregateZero :: BenchTest
aggregateZero = let expectedVars = M.fromList [ ("add_3", 0)
                                              , ("add_4", 0)
                                              ]
         in variableAssignmentTest expectedVars $ constantPath ++ "aggregateZero.ll"

struct :: BenchTest
struct = let expectedVars = M.fromList [ ("simple_2", 100)
                                       , ("simple_3", 200)
                                       , ("simple_7", 100)
                                       , ("simple_8", 200)
                                       , ("simple_12", 0)
                                       , ("simple_13", 0)
                                       ]
         in variableAssignmentTest expectedVars $ constantPath ++ "struct.ll"

array :: BenchTest
array = let expectedVars = M.fromList [ ("simple_letter", 101) -- e
                                      , ("simple_letter1", 97) -- a
                                      ]
         in variableAssignmentTest expectedVars $ constantPath ++ "array.ll"

vector :: BenchTest
vector = let expectedVars = M.fromList [ ("add_3", 0)
                                       , ("add_4", 1)
                                       , ("add_5", 2)
                                       , ("add_6", 3)
                                       ]
         in variableAssignmentTest expectedVars $ constantPath ++ "vector.ll"

undef :: BenchTest
undef = variableAssignmentTest (M.fromList []) $ constantPath ++ "undef.ll"

globalVariable :: BenchTest
globalVariable = let expectedVars = M.fromList [ ("simple_3", 10)
                                               , ("simple_4", 20)
                                               , ("simple_6", 440)
                                               , ("simple_9", 0)
                                               , ("simple_10", 1)
                                               ]
                 in variableAssignmentTest expectedVars $ constantPath ++ "globalVar.ll"

-- Operations

binOp :: BenchTest
binOp = let expectedVars = M.fromList [ ("simple_2", 5)
                                      , ("simple_3", 15)
                                      , ("simple_5", 255)
                                      , ("simple_6", 0)
                                      , ("simple_8", 25)
                                      , ("simple_9", 0)
                                      , ("simple_11", 127)
                                      , ("simple_12", 127)
                                      , ("simple_14", 255)
                                      , ("simple_15", 255)
                                      , ("simple_17", 1)
                                      , ("simple_18", 255)
                                      , ("simple_20", 2)
                                      , ("simple_21", 1)
                                      , ("simple_23", 255)
                                      , ("simple_25", 0)
                                      , ("simple_26", 1)
                                      , ("simple_28", 1)
                                      ]
        in variableAssignmentTest expectedVars $ constantPath ++ "binOp.ll"

castOp :: BenchTest
castOp = let expectedVars = M.fromList [ ("simple_2", 255)
                                       , ("simple_3", 1)
                                       , ("simple_5", 255)
                                       , ("simple_6", 0)
                                       , ("simple_8", 5)
                                       , ("simple_10", 0)
                                       ]
         in variableAssignmentTest expectedVars $ constantPath ++ "castOp.ll"

getElementPtr :: BenchTest
getElementPtr = let expectedVars = M.fromList [ ("simple_val1", 10)
                                              , ("simple_val2", 33)
                                              , ("simple_val3", 82)
                                              ]
                in variableAssignmentTestAlias expectedVars $ constantPath ++ "gep.ll"

-- Constant comparisons use the same translatePredicate function that
-- instruction comparisons use, so we don't have to test every kind
-- of translation (just one!)
cmpOp :: BenchTest
cmpOp = let expectedVars = M.fromList [ ("simple_2", 1)
                                      , ("simple_3", 1)
                                      , ("simple_6", 0)
                                      , ("simple_7", 1)
                                      ]
        in variableAssignmentTest expectedVars $ constantPath ++ "cmpOp.ll"

selectOp :: BenchTest
selectOp = let expectedVars = M.fromList [ ("simple_2", 1)
                                         , ("simple_3", 0)
                                         , ("simple_6", 1)
                                         , ("simple_8", 0)
                                         , ("simple_11", 2)
                                         ]
           in variableAssignmentTest expectedVars $ constantPath ++ "selectOp.ll"

insertElement :: BenchTest
insertElement = let expectedVars = M.fromList [ ("simple_3", 1)
                                              , ("simple_5", 500)
                                              , ("simple_9", 50)
                                              ]
                in variableAssignmentTest expectedVars $ constantPath ++ "insertElemOp.ll"

extractElement :: BenchTest
extractElement = let expectedVars = M.fromList [ ("simple_result", 2) ]
                 in variableAssignmentTest expectedVars $ constantPath ++ "extractElemOp.ll"

shuffleVector :: BenchTest
shuffleVector = let expectedVars = M.fromList [ ("simple_fourhundred", 400)
                                              , ("simple_threehundred", 300)
                                              ]
                in variableAssignmentTest expectedVars $ constantPath ++ "shufflevector.ll"

insertValue :: BenchTest
insertValue = let expectedVars = M.fromList [ ("simple_val_one", 1)
                                            , ("simple_val_two_1", 100)
                                            , ("simple_val_three", 100)
                                            , ("simple_val_four", 100)
                                            , ("simple_val_five", 100)
                                            ]
              in variableAssignmentTest expectedVars $ constantPath ++ "insertValOp.ll"

extractValue :: BenchTest
extractValue = let expectedVars = M.fromList [ ("simple_2", 200)
                                             , ("simple_3", 200)
                                             , ("simple_6", 1000)
                                             ]
               in variableAssignmentTest expectedVars $ constantPath ++ "extractValOp.ll"
