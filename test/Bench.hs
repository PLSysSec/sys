module Main where

import           AllTests (allTests)
import           BenchUtils
import           Criterion.Main

main :: IO ()
main = defaultMain $ map getBench allTests
