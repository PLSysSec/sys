{-|

Module that automatically runs a user's static checker.

staticCheckFunctionAsyn sets up the static checker's internal state given the user's
configuration object. Then, it calls checkPaths to check every path in the given
file.

checkPaths automatically applies the user's static checker to each path in the file.

-}
module Static.Check (staticCheckFunctionAsync) where

import           Control.Concurrent(setNumCapabilities)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Thread.Group
import           Control.Monad.State.Strict      (forM_, unless, void)
import           Control.Monad.State.Strict      (get, liftIO)
import           Data.Maybe
import           InternalIR.ModuleInfo
import           LLVM.AST
import           LLVMAST.Interface
import           Prelude                         hiding (mod)
import           Static.CheckerConfigDef
import           Static.CheckerState

-- | Setup an internal checker state and automatically run the user's static checker
staticCheckFunctionAsync :: (Show a, Show b, Ord b)
                         => TQueue b
                         -> ThreadGroup
                         -> FilePath
                         -> ModuleInfo
                         -> Definition
                         -> CheckerConfig a b
                         -> IO ()
staticCheckFunctionAsync tQueue tg filepath mod def config
  | isJust mBasicBlocks && not (null basicBlocks) = do
      let cfg0s = stateFromConfig filepath mod def config
          cfg1s = map (\cfg -> cfg { curBugs = tQueue, curTGroup = tg }) cfg0s
          act = do cfgInitialAction config
                   checkPaths config
                   cfgFinalAction config
      forM_ cfg1s $ \cfg1 -> evalChecker act cfg1
  | otherwise = return ()
  where
    mBasicBlocks = getFunctionBBs def
    basicBlocks = fromJust mBasicBlocks

-- | Statically check all paths up to a bound specified by config.
-- checkPaths applies the user's static checking function, also found in config,
-- to each line of LLVM IR on each path. To do so, checkPaths creates an LLVM IR
-- control flow graph on the fly (getNextBlocks).
checkPaths :: (Show a, Show b)
           => CheckerConfig a b
           -> Checker a b ()
checkPaths config = do
  -- Perform the check on each instruction in the block
  block <- getCurBlock
  path <- getPath
  forM_ (zip [0..] $ getBlockContents block) $ \(num, ninstr) -> cfgCheck config num ninstr
  -- Figure out if we should keep exploring this path
  nextBlocks <- getNextBlocks
  bound <- getBlockBound
  maxLoops <- getLoopBound
  -- Don't continue if the path is too long
  unless (length path >= bound) $ do
    forM_ nextBlocks $ \nextBlock -> fork $ do
      -- Don't continue if we've been through this loop too many times
      unless (occurs nextBlock path >= maxLoops) $ do
        -- Go to the next block and check it
        advanceTo nextBlock
        checkPaths config

-- | Fork a thread in checker monad.
fork :: Checker a b () -> Checker a b ()
fork act = do
  s0 <- get
  void $ liftIO $ do
    setNumCapabilities 1 -- number of Threads
    forkIO (curTGroup s0) $ evalChecker act s0

occurs :: Eq a => a -> [a] -> Int
occurs x = length . filter (x==)
