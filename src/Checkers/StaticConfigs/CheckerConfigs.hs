module Checkers.StaticConfigs.CheckerConfigs where
import           Checkers.ConcreteOOBStatic
import           Checkers.HeapOOBStatic
import           Checkers.UAFStatic
import           Checkers.UninitStatic
import           Checkers.UserInputStatic
import qualified Data.ByteString            as B
import           Data.ByteString.UTF8       (fromString)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           LLVM.AST
import           LLVMAST.Interface
import           Static.CheckerConfigDef

-- | Start at the top of the function
startAtTop :: M.Map Name BasicBlock
           -> [BasicBlock]
           -> [Name]
startAtTop _ blocks = [getBlockName $ head blocks]

uninitConfig :: CheckerConfig Uninits UninitBug
uninitConfig =
  CheckerConfig { cfgShouldCheckFile = \file -> fromString "alloca" `B.isInfixOf` file
                , cfgShouldCheckModule = \llvmMod -> "Alloca" `isInfixOf` show llvmMod
                , cfgShouldCheckFunction = \fun -> "Alloca" `isInfixOf` show fun
                , cfgVerbose = True
                , cfgBlockBound = 6
                , cfgLoopBound = 2
                , cfgDebugPath = Nothing
                , cfgAccumState = False
                , cfgStartState = blankUninit
                , cfgCheck = uninitCheck Stack
                , cfgInitialAction = return ()
                , cfgFinalAction = return ()
                , cfgDieOnBug = True --False
                , cfgGetStarts = startAtTop
                , cfgGoBackBy = 0
                }

userInputConfig :: CheckerConfig InputBasicState InputBug
userInputConfig =
  CheckerConfig { cfgShouldCheckFile = \f -> fromString "copyin" `B.isInfixOf` f
                , cfgShouldCheckModule = \m -> "copyin" `isInfixOf` show m
                , cfgShouldCheckFunction = \f -> "copyin" `isInfixOf` show f
                , cfgVerbose = True
                , cfgBlockBound = 20
                , cfgLoopBound = 20
                , cfgDebugPath = Nothing
                , cfgAccumState = False
                , cfgStartState = blankInputState
                , cfgCheck = userInputCheck
                , cfgInitialAction = return ()
                , cfgFinalAction = return ()
                , cfgDieOnBug = False
                , cfgGetStarts = startAtTop
                , cfgGoBackBy = 0
                }

heapoobConfig :: CheckerConfig OOBState MOOBBug
heapoobConfig =
  CheckerConfig { cfgShouldCheckFile = \file -> fromString "alloc" `B.isInfixOf` file
                , cfgShouldCheckModule = \llvmMod -> "alloc" `isInfixOf` show llvmMod
                , cfgShouldCheckFunction = \fun -> "alloc" `isInfixOf` show fun
                , cfgVerbose = True
                , cfgBlockBound = 15
                -- So the paper regressions can finish on a laptop
                , cfgLoopBound = 1
                , cfgDebugPath = Nothing
                , cfgAccumState = False
                , cfgStartState = blankOOBState
                , cfgCheck = moobCheck
                , cfgInitialAction = return ()
                , cfgFinalAction = return ()
                , cfgDieOnBug = False
                , cfgGetStarts = startAtTop
                , cfgGoBackBy = 0
                }

concroobConfig :: CheckerConfig COOBInfo COOBBug
concroobConfig =
  CheckerConfig { cfgShouldCheckFile = const True
                , cfgShouldCheckModule = const True
                , cfgShouldCheckFunction = const True -- \f -> "AddAudioTrack" `isInfixOf` (show $ getFunctionName f)
                , cfgVerbose = True
                , cfgBlockBound = 0
                , cfgLoopBound = 0
                , cfgDebugPath = Nothing
                , cfgAccumState = False
                , cfgStartState = blankInfo
                , cfgCheck = \_ _ -> return ()
                , cfgInitialAction = checkAll
                , cfgFinalAction = return ()
                , cfgDieOnBug = False
                , cfgGetStarts = startAtTop
                , cfgGoBackBy = 0
                }

uafConfig :: CheckerConfig UAFState UAFBug
uafConfig =
  CheckerConfig { cfgShouldCheckFile = \f -> isFreeB f
                , cfgShouldCheckModule = \m -> isFree m
                , cfgShouldCheckFunction = \f -> isFree f
                , cfgVerbose = True
                , cfgBlockBound = 15
                , cfgLoopBound = 1
                , cfgDebugPath = Nothing
                , cfgAccumState = False
                , cfgStartState = blankUAFState
                , cfgCheck = uafCheck
                , cfgInitialAction = return ()
                , cfgFinalAction = return ()
                , cfgDieOnBug = False
                , cfgGetStarts = startAtTop
                , cfgGoBackBy = 0
                }
  -- NOTE: Look for free (overapproximate)
  where isFreeB f = fromString "free" `B.isInfixOf` f ||
                    fromString "_ZdlPv" `B.isInfixOf` f ||
                    fromString "_ZdaPv" `B.isInfixOf` f
        isFree f = "free" `isInfixOf` show f ||
                   "_ZdlPv" `isInfixOf` show f ||
                   "_ZdaPv" `isInfixOf` show f
