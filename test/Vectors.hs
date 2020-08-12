{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Vectors where
import qualified Data.Map as M
import           Utils

vectorPath :: String
vectorPath = "test/Backend/Vectors/"

vectorTests :: BenchTest
vectorTests = benchTestGroup "Vector versions of instructions for scalars"
              [ addO3
              , truncO3
              , selectO3
              , icmpO3
              ]

addO3 :: BenchTest
addO3 = let expectedVars = M.fromList [ ("add_4", 0) -- 0 + 0
                                      , ("add_5", 2) -- 1 + 1
                                      , ("add_6", 4) -- 2 + 2
                                      , ("add_7", 6) -- 3 + 3
                                      ]
        in variableAssignmentTest expectedVars $ vectorPath ++ "add_O3.ll"

truncO3 :: BenchTest
truncO3 = let expectedVars = M.fromList [ ("trunc_4", 0)
                                        , ("trunc_5", 1)
                                        ]
          in variableAssignmentTest expectedVars $ vectorPath ++ "trunc_O3.ll"

selectO3 :: BenchTest
selectO3 = let expectedVars = M.fromList [ ("or_4", 1)
                                         , ("or_5", 1)
                                         , ("or_8", 1)
                                         , ("or_9", 1)
                                         ]
           in variableAssignmentTest expectedVars $ vectorPath ++ "select_O3.ll"

icmpO3 :: BenchTest
icmpO3 = let expectedVars = M.fromList [ ("or_5", 1)
                                       , ("or_6", 0)
                                       , ("or_7", 0)
                                       , ("or_8", 1)
                                       ]
         in variableAssignmentTest expectedVars $ vectorPath ++ "icmp_O3.ll"
