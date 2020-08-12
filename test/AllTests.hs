module AllTests (allTests) where
import           Constants       (constantTests)
import           Instructions    (instructionTests)
import           PathPrefix      (pathPrefixTests)
import           Pathsearch      (pathSearchTests)
import           Regression      (regressionTests)
import           SSA             (ssaTests)
import           RenameVariables (renameVariablesTests)
import           Representation  (representationTests)
import           StaticPath      (staticPathTests)
import           Types           (typeTests)
import           Vectors         (vectorTests)

import           BenchUtils

frontendTests :: BenchTest
frontendTests = benchTestGroup "Frontend" [ renameVariablesTests
                                          , ssaTests
                                          , pathSearchTests
                                          , representationTests
                                          , pathPrefixTests
                                          , staticPathTests
                                          ]

backendTests :: BenchTest
backendTests = benchTestGroup "Backend" [ instructionTests
                                        , constantTests
                                        , vectorTests
                                        , typeTests
                                        ]

endToEndTests :: BenchTest
endToEndTests = benchTestGroup "End-to-end" [ regressionTests ]

allTests :: [BenchTest]
allTests = [ frontendTests
           , backendTests
           , endToEndTests
           ]
