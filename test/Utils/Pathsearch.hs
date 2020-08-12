{- |

Symbolically execute a file. This is a more classical
implementation of symex than our static/symbolic approach.

-}

module Utils.Pathsearch ( symexFile ) where

import           Control.Concurrent.MVar
import           Control.Concurrent.Thread.Group
import           Control.Exception               as E
import           Control.Monad
import           Data.ByteString                 (readFile)
import           LLVM.AST
import           LLVM.Context
import           LLVMAST.Interface
import           InternalIR.ModuleInfo
import           InternalIR.PathInfo
import           Utils.Pathsearch.Config
import           Utils.Pathsearch.Pathsearch
import           Prelude                         hiding (log, pred, readFile)
import           Symex.Symex
import qualified Symex.CheckerConfigDef          as SymConfig

-- | Symbolically execute everything in a file.
-- This is mostly used for testing, since for real use cases we almost always
-- symbolically execute a path that is pre-determined by the static analysis pass
-- Note: threaded for testing
symexFile ::  SymConfig.Config a   -- ^ Symex config
          -> FilePath   -- ^ Symex paths which *start* in this file
          -> [FilePath] -- ^ (Optional) additional files to follow paths into.
                        -- But we don't *start* in these files.
          -> IO [SolverResult]
symexFile config path xtraPaths = do
  fileGroup  <- new
  allResults <- newMVar []
  contents <- readFile path
  -- Symex the file
  when (SymConfig.symexContents config contents) $ do  -- should we symex the file?
    withContext $ \ctx -> E.handle printHandler $ do
      modInfo <- getModuleInfo ctx contents
      xtraMods <- mapM (getModuleInfoFromFile ctx) xtraPaths
      when (SymConfig.symexAST config $ modAST modInfo) $  -- should we symex the module?
        forM_ (getFunctionDefns $ modAST modInfo) $ \defn ->
          when (SymConfig.symexDefn config defn) $  -- should we symex the function?
            symexFunction fileGroup allResults config (getFunctionName' defn) modInfo xtraMods
  -- Wait for all symex threads to finish
  wait fileGroup
  -- Get all the results
  readMVar allResults
    where getFunctionDefns :: Module -> [Definition]
          getFunctionDefns = filter isFunction . moduleDefinitions

-- | Symbolically execute a function
-- Again, threaded for testing
symexFunction :: ThreadGroup
              -> MVar [SolverResult]
              -> SymConfig.Config a
              -> Name
              -> ModuleInfo
              -> [ModuleInfo]
              -> IO ()
symexFunction tGroup allResults config funcName modInfo xtraMods = do
  let psCfg0 = pathCfgWithLength $ SymConfig.pathLength config
      psCfg1 = psCfg0 { psEnterCalls = dynCallCfg $ \_ _ -> SymConfig.callsOn config
                      , psFollowRets = if SymConfig.callsOn config
                                       then SymConfig.returnFollowOn config
                                       else False
                      }
  -- Get the pathsearch thread group and queue
  forEachSimplePath psCfg1 funcName modInfo xtraMods $ \path -> void $ forkIO tGroup $ do
   when (SymConfig.symexPath config path) $ do
      -- get the path info
      let pathInfo = getPathInfo modInfo (SymConfig.pathLength config) funcName path $
                     SymConfig.arrayBound config
      -- symex the path
      E.handle (\err -> printHandler err >> appendResult (SolverFailed $ show err)) $ do
        result <- symex config path pathInfo
        appendResult result
   where appendResult r = modifyMVar_ allResults $ return . (r:)

printHandler :: E.SomeException -> IO ()
printHandler = print
