-- | Helper functions
module Utils (
    -- * Utils
      getSourceFiles
    --- ** Checker specific printers
    , showUninitResult
    , showMallocResult
    , showUserInputResult
    , showUAFResult
    , showNegBug
    ) where
import           Checkers.ConcreteOOBStatic
import           Checkers.HeapOOBStatic
import           Checkers.UAFStatic
import           Checkers.UninitStatic
import           Checkers.UserInputStatic
import           Control.Monad
import           Prelude                    hiding (log, pred, readFile)
import           Symex.Symex
import           System.Directory           (canonicalizePath, getCurrentDirectory)
import           System.FilePath            (isAbsolute, (</>))
import           System.FilePath.Find


-- | Where to print bugs. May be useful to use Control.Log.log  when debugging.
printBug :: String -> IO ()
printBug = putStrLn

--
-- Printing results
--

showUninitResult :: UninitBug -> SolverResult -> IO ()
showUninitResult (UninitBug no path _ fname fp var _) result = when (isSat result) $
  printBug $ unlines ["Stack uninit bug"
                     , show var
                     , "in"
                     , show fname
                     , show path
                     , show fp
                     , "on line"
                     , show no
--                     , show result
                     ]

showMallocResult :: MOOBBug -> SolverResult -> IO ()
showMallocResult (MOOBBug (OOBInfo var asize isize jsize _ oobName oline opath)
                          fp fun p line) result = when (isSat result) $
      printBug $ unlines [ "Out of bounds off of"
                         , show var
                         , "via var"
                         , show oobName
                         , show oline
                         , show opath
                         , "Of allocation size"
                         , show asize
                         , "and jump and index size"
                         , unwords [show isize, show jsize]
                         , "is"
                         , showAttackResult result
                         , show result
                         , "on line"
                         , show line
                         , "on path"
                         , show p
                         , "in function"
                         , show fun
                         , "in file"
                         , fp
                         ]

showUserInputResult :: InputBug -> SolverResult -> IO ()
showUserInputResult (InputBug fp fname path _ val var) result = when (isSat result) $
  printBug $ unlines [ "Potential oob with user input"
                     , show var
                     , "with size"
                     , show val
                     , "is"
                     , showAttackResult result
                     , show result
                     , "in file"
                     , fp
                     , "in function"
                     , show fname
                     , "on path"
                     , show path
                     ]

showNegBug :: COOBBug -> SolverResult -> IO ()
showNegBug (COOBBug oobVar negIdx negAmt path fname fp) result = when (isSat result) $
  printBug $ unlines [ "Potential OOB index of"
                     , show negIdx
                     , show negAmt
                     , show oobVar
                     , "is"
                     , showPathResult [result]
                     , "in file"
                     , fp
                     , "in function"
                     , show fname
                     , "on path"
                     , show path
                     ]

showUAFResult :: UAFBug -> SolverResult -> IO ()
showUAFResult (UAFBug fp fname path lineno var _ freename) result = when (isSat result) $
  printBug $ unwords ["UAF bug:", "Variable ", show var, "in function"
                     , show fname, "of", show fp, "on"
                     , "line", show lineno, "on path:", show path
                     , "with free function", show freename
                     ]



showAttackResult :: SolverResult -> String
showAttackResult sr = case sr of
                        SolverSat{}    -> "attack possible"
                        SolverUnsat{}  -> "attack impossible"
                        SolverFailed e -> unwords ["attack failed with", e]

showPathResult :: [SolverResult] -> String
showPathResult [sr] = case sr of
                    SolverSat{}    -> "path possible"
                    SolverUnsat{}  -> "path impossible"
                    SolverFailed e -> unwords ["path failed with", e]
showPathResult [] = "empty path result"
showPathResult res = "unexpectedly large number of path results: " ++ show (length res)

isSat :: SolverResult -> Bool
isSat SolverSat{} = True
isSat _           = False

--
-- Getting files
--

-- | Get all source files if dir0 is a directory, otherwise get file dir0
getSourceFiles :: FilePath -> [String] -> IO [FilePath]
getSourceFiles dir0 extns = do
  dir <- if isAbsolute dir0
         then return dir0
         else getCurrentDirectory >>= canonicalizePath . (</> dir0)
  getSourceFilesWithExtns extns dir

-- | Get all the source files given a list of extensions and absolute path
getSourceFilesWithExtns :: [String] -> FilePath -> IO [FilePath]
getSourceFilesWithExtns extns = find always (fileType ==? RegularFile &&? extnsPred)
  where extnsPred = foldl1 (||?) $ map (\extn -> extension ==? extn) extns
