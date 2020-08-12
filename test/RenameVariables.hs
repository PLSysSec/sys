{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module RenameVariables ( renameVariablesTests ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import LLVM.Pretty
import InternalIR.ModuleInfo

import Utils

renameVariablesTests :: BenchTest
renameVariablesTests = benchTestGroup "Rename variable tests" [ goldenBenchTest "test.ll" ]

goldenBenchTest :: String -> BenchTest
goldenBenchTest name = let path = renameVariablesPath ++ name
                       in benchGoldenVsString name (path ++ ".gold") (readAndRenameModule path)


renameVariablesPath :: String
renameVariablesPath = "test/RenameVariables/"

readAndRenameModule :: FilePath -> IO ByteString
readAndRenameModule path = withModule path $ \mInfo ->
  return $ encodeUtf8 $ ppllvm $ modAST mInfo
