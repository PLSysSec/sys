module Main where
import           Control.Log
import           Data.List             (sort)
import           Lib
import           Prelude               hiding (log)
import           System.Console.GetOpt
import           System.Environment

checkFunction :: [String] -> String -> (FilePath -> IO ())
checkFunction extns opt =
  case opt of
    "uninit"    -> \filepath -> uninit filepath extns
    "heapoob"   -> \filepath -> heapOOB filepath extns
    "concroob"  -> \filepath -> concrOOB filepath extns
    "userinput" -> \filepath -> userInput filepath extns
    "uaf"       -> \filepath -> uaf filepath extns
    _           -> error "Unknown checker!"


data Flag = Dir { dirOpt :: String }
          | Extn { extnOpt :: String }
          | Checker { ckerOpt :: String }
          deriving (Eq, Ord)

options :: [OptDescr Flag]
options =
    [ Option ['d']     ["libdir"]   (ReqArg Dir "DIR") "directory (or file) to analyze"
    , Option ['e']     ["extn"]     (ReqArg Extn "EXTN" ) "file extension"
    , Option ['c']     ["check"]    (ReqArg Checker "CHECK") "checker to run"
    ]

makeExtns :: String -> [String]
makeExtns "ll"   = [".ll", ".bc"]
makeExtns "O0"   = [".ll-O0", ".ll-O0_p", ".bc-O0_p"]
makeExtns "O1"   = [".ll-O1", ".ll-O1_p", ".bc-O1_p"]
makeExtns "O2"   = [".ll-O2", ".ll-O2_p", ".bc-O2_p"]
makeExtns "O3"   = [".ll-O3", ".ll-O3_p", ".bc-O3_p"]
makeExtns "Og"   = [".ll-Og", ".ll-Og_p", ".bc-Og_p"]
makeExtns "Os"   = [".ll-Os", ".ll-Os_p", ".bc-Os_p"]
makeExtns "Oz"   = [".ll-Oz", ".ll-Os_z", ".bc-Os_z"]
makeExtns "prod" = map (++ "_p") allExtns
makeExtns "all"  = allExtns
makeExtns extn   = [extn]

allExtns :: [String]
allExtns = [ ".ll"
           , ".ll-O0"
           , ".ll-O1"
           , ".ll-O2"
           , ".ll-O3"
           , ".ll-Oz"
           , ".ll-Os"
           , ".ll-Og"
           , ".ll-Ofast"
           , ".bc"
           , ".bc-O0"
           , ".bc-O1"
           , ".bc-O2"
           , ".bc-O3"
           , ".bc-Oz"
           , ".bc-Os"
           , ".bc-Og"
           , ".bc-Ofast"
           ]

main :: IO ()
main = withStdOutLogger $ do
  argv <- getArgs
  case getOpt Permute options argv of
    (flags,_,[]) -> case sort flags of
                      flist | length flist < 3 -> ioError $ userError $ "Not enough flags" ++
                                              usageInfo header options
                      flist -> do
                        let dir      = dirOpt $ flist !! 0
                            extns    = makeExtns $ extnOpt $ flist !! 1
                            check    = checkFunction extns $ ckerOpt $ flist !! 2
                        check dir
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
  where header = "Usage: "
