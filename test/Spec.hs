module Main where

import           AllTests   (allTests)
import           BenchUtils
import           Test.Tasty
--For Logging: import Control.Log

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

-- For Logging:
-- withStdOutLogger $ defaultMain $
--  testGroup "All tests" $ map getTest allTests
