{- |

This module implements a simple 'StaticPath' refinement 'enterCalls' that
enters/inlines function calls.

-}
module InternalIR.PathExpand.Refinements.EnterCall (enterCalls, enterCall) where

import           Control.Concurrent              (killThread, myThreadId)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Thread.Group
import           Control.Monad.State.Strict
import           Control.Monad.STM               (atomically)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, fromJust)
import           InternalIR.PathExpand
import           InternalIR.SimplePath
import           LLVM.AST
import           LLVMAST.ASTInterface

-- | Given a static path produce new static paths where each call is entered.
enterCalls :: StaticPath   -- ^ Entry corresponding to call
           -> ProgramInfo  -- ^ Module info of functions we're entering
           -> Int          -- ^ Loop bound
           -> IO [StaticPath]
enterCalls staticPath pInfo loopBound = do
  let callSPEs = filter (isCallInstr . snd) numberedPath
  mEnterdCalls <- forM callSPEs $ \(nr, spe) ->
    case getFunctionInAnyModule pInfo (calledFuncName spe) of
      Just (modInfo, _) -> do sps <- enterCall spe modInfo loopBound
                              return $ Just (nr, sps)
      Nothing -> return Nothing
  return $ map (map snd) $ spliceInto [numberedPath] $ catMaybes mEnterdCalls

  where numberedPath = zip [0..] staticPath
        -- For each numbered static path spliace the next call and go to the next one
        spliceInto :: [NumberedStaticPath] -> [(Int, [StaticPath])] -> [NumberedStaticPath]
        spliceInto curNPs [] = curNPs
        spliceInto curNPs (callNPEs:rest) =
          let nextNPs = concatMap (spliceCallAt callNPEs) curNPs
          in spliceInto nextNPs rest

-- | Replace a call instruction numbered nr with a call-entry path. Do it for
-- all paths in function.
spliceCallAt :: (Int, [StaticPath])
             -> NumberedStaticPath
             -> [NumberedStaticPath]
spliceCallAt (nr, sps) nsp =
  let (prefix, (_:suffix)) = break ((==nr) . fst) nsp
  in map (\p -> prefix ++ p ++ suffix) nsps
  where nsps :: [NumberedStaticPath]
        nsps = map (zip (repeat nr)) sps

type NumberedStaticPath = [(Int, StaticPathEntry)]

isCallInstr :: StaticPathEntry -> Bool
isCallInstr spe = case namedInstrFromEntry spe of
  Just ni -> isCallInstruction ni
  _       -> False

-- | Enter a particular call instruction.
enterCall :: StaticPathEntry   -- ^ Entry corresponding to call
          -> ModuleInfo        -- ^ Module info of function we're entering
          -> Int               -- ^ Loop bound
          -> IO [StaticPath]
enterCall spe modInfo loopBound | isCallInstr spe = do
    queue <- newTQueueIO
    tg <- new
    let s0 = WalkerState {  wCallingBB = speBasicBlockName spe
                          , wMod       = modInfo
                          , wFunc      = funcName
                          , wLoopBound = loopBound
                          , wBBMap     = makeBBMap funcBBs
                          , wTGroup    = tg
                          , wQueue     = queue
                          , wCurPath   = []
                          , wPrevBB    = Nothing
                          }
    -- Fork a thread for each function basic block and start walking each block
    void $ forkIO tg $ evalStateT (enterBlock (getBlockName $ head funcBBs)) s0
    -- Wait for all threads.
    wait tg
    -- Flush the queue and return unique paths
    paths <- atomically $ flushTQueue queue
    -- Prefix the call instruction which will result in the staticToSimple to
    -- stitch calls accordingly
    return $ map (initCallInstr:) paths
    where funcName = calledFuncName spe
          funcBBs = fromJust $ bbsFromFunctionName (modAST modInfo) funcName
          -- Simple path entry representing Call intruction
          idx = speInstrIndex . speIndex $ spe
          initCallInstr = spe { speIndex = SPECall idx True }
enterCall _ _ _ = error "BUG: invalid use of enterCall, expected SPECall"

calledFuncName :: StaticPathEntry -> Name
calledFuncName = fromJust . getFunName . withoutName . namedInstrFromEntry'

enterBlock :: Name -> Walker ()
enterBlock bbName = do
  -- Check entering this block would exceed the loop bound, and bail if so
  ok <- checkLoopBound bbName
  when ok $ do
    bb <- getBasicBlock bbName
    -- Add terminator and continue to next block
    case withoutName (getBlockTerminator bb) of
      Ret {} -> do callingBB <- wCallingBB <$> get
                   addBBToPath bb callingBB
                   done
      CondBr _ trueBr falseBr _ -> do
        fork $ addBBToPath bb trueBr  >> enterBlock trueBr
        fork $ addBBToPath bb falseBr >> enterBlock falseBr
        die
      Br dst _ -> enterBlock dst
      Switch _ defDst otherDsts _ -> do
        forM_ (defDst:(map snd otherDsts)) $ \dst -> fork $ do
          addBBToPath bb dst
          enterBlock dst
        die
      IndirectBr _ dsts _  -> do
        forM_ dsts $ \dst -> fork $ do
          addBBToPath bb dst
          enterBlock dst
        die
      -- Not including the other terminators for now
      term  -> error $ "Terminator not yet supported: " ++ show term

-- | Kill current thread
die :: Walker ()
die = liftIO $ do
  tid <- myThreadId
  killThread tid

-- | Write result and die
done :: Walker ()
done = do
  s0 <- get
  let sp = mconcat $ map snd $ wCurPath s0
  -- Write the path if it's not null
  unless (null sp) $
    liftIO $ atomically $ writeTQueue (wQueue s0) sp
  -- We're done, die
  die


-- | Fork a thread wherein the walking will continue.
fork :: Walker () -> Walker ()
fork act = do
  s0 <- get
  void $ liftIO $ forkIO (wTGroup s0) $ void $ execStateT act s0


-- | Check if loop bound is still okay
checkLoopBound :: Name -> Walker Bool
checkLoopBound bbName = do
  curPath <- wCurPath <$> get
  let nrOccur = length (filter (\(n,_) -> n == bbName) curPath)
  loopBound <- wLoopBound <$> get
  return $ nrOccur < loopBound

-- | Add basic block to the path.
addBBToPath :: BasicBlock -> Name -> Walker ()
addBBToPath bb nextBBName = do
  s0 <- get
  let bbName = getBlockName bb
      spe0 = StaticPathEntry { speModule         = wMod s0
                             , speFunction       = wFunc s0
                             , speBasicBlockName = bbName
                             , spePreviousBlock  = wPrevBB s0
                             , speIndex          = undefined
                             }
      nrInstr   = length $ getBlockContents bb
      instrSPEs = if nrInstr > 0
                    then map (\idx -> spe0 { speIndex = SPEInstr idx }) [0.. nrInstr-1]
                    else []
      termSPE   = [spe0 { speIndex = SPETerm nextBBName }]
  put $ s0 { wCurPath = wCurPath s0 ++ [(bbName, instrSPEs ++ termSPE)] }

data WalkerState = WalkerState
  { wCallingBB :: Name                  -- ^ The basic block where the call is from
  , wMod       :: ModuleInfo            -- ^ Module
  , wFunc      :: Name                  -- ^ Function name
  , wLoopBound :: Int                   -- ^ Loop bound
  , wBBMap     :: M.Map Name BasicBlock -- ^ Cached BB map
  -----------------------------------------------------------------
  , wTGroup    :: ThreadGroup           -- ^ Thread group
  , wQueue     :: TQueue StaticPath     -- ^ Queue of all paths
  -----------------------------------------------------------------
  , wCurPath   :: [(Name, StaticPath)]  -- ^ Static path (thread local)
  , wPrevBB    :: Maybe Name            -- ^ Previous block
  }

-- | Walker monad is simply a statemonad (over IO)
type Walker = StateT WalkerState IO

-- | Given basic block name, get the next basic blocks in current function.
getBasicBlock :: Name -> Walker BasicBlock
getBasicBlock bbName = do
  bbMap <- wBBMap <$> get
  return $ bbMap M.! bbName
