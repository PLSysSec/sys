{- |

This module implements a very simple thread-safe logger.
Set the `LOG_LEVEL` environment variable (to one ofthe 'LogLevel's) to set
the initial log level.

-}
module Control.Log ( withStdOutLogger
                   , log
                   , whenLogLevel
                   , LogLevel(..)
                   , setLogLevel
                   ) where

import           Data.Char              (toLower)
import           Data.IORef

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class

import           System.Environment     (lookupEnv)
import           System.IO.Unsafe

import           Text.Printf

import           Prelude                hiding (log)

-- | Log levels, ordered so that anything printed at 'INFO' level is also
-- printed at 'DEBUG' level, etc.
data LogLevel = VVERBOSE  -- includes solver verbosity
              | VERBOSE
              | DEBUG
              | INFO
              | WARN
              | EVAL
              | ERROR
              | SILENT
              deriving (Eq, Ord, Show)

-- | Program log level.
logLevel :: IORef LogLevel
{-# NOINLINE logLevel #-}
logLevel = unsafePerformIO $ newIORef INFO

-- | Set the log level.
setLogLevel :: MonadIO m => LogLevel -> m ()
setLogLevel ll = liftIO $ atomicWriteIORef logLevel ll

-- | Get the log level.
getLogLevel :: MonadIO m => m LogLevel
getLogLevel = liftIO $readIORef logLevel

logChannel :: TChan (ThreadId, String)
{-# NOINLINE logChannel #-}
logChannel = unsafePerformIO $ newBroadcastTChanIO

-- | Initialize new logging thread.  This function should only be called once.
withStdOutLogger :: MonadIO m => m a -> m a
withStdOutLogger act = do
  liftIO $ do
    maybeSetLogLevel
    ch <- atomically $ dupTChan logChannel
    void . forkIO $ forever $ do
      (tid, str) <- atomically $ readTChan ch
      forM_ (lines str) $ \line -> printf "%s :: %s\n" (show tid) line
  act
    where maybeSetLogLevel = do
            mll <- lookupEnv "LOG_LEVEL"
            case mll of
              Nothing -> return ()
              Just ll -> case map toLower ll of
                "vverbose" -> setLogLevel VVERBOSE
                "verbose"  -> setLogLevel VERBOSE
                "debug"    -> setLogLevel DEBUG
                "info"     -> setLogLevel INFO
                "warn"     -> setLogLevel WARN
                "eval"     -> setLogLevel EVAL
                "error"    -> setLogLevel ERROR
                "silent"   -> setLogLevel SILENT
                _          -> error $ "Invalid LOG_LEVEL " ++ show ll

-- | Log something, in IO.
log :: MonadIO m =>  LogLevel -> String -> m ()
log ll str = liftIO $ do
  curLL <- getLogLevel
  when (ll >= curLL) $ do
    tid <- myThreadId
    atomically $ writeTChan logChannel (tid, str)

-- | Execute an action when log-level is at at least @ll@.
whenLogLevel :: MonadIO m => LogLevel -> m () -> m ()
whenLogLevel ll act = do
  curLL <- getLogLevel
  when (ll >= curLL) act

