module Utils.Pathsearch.Pathsearch ( getAllSimplePaths
                                   , forEachSimplePath ) where

import           Control.Monad.State.Strict
import           Control.Monad.STM (atomically, retry)
import           Control.Concurrent (myThreadId, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Thread.Group
import           Data.List                 (elemIndices, nub, isPrefixOf)
import           Data.Maybe                (maybeToList, mapMaybe, isJust, fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set
import           LLVM.AST
import qualified LLVM.AST.Constant as C
import           LLVMAST.ASTInterface
import           InternalIR.ModuleInfo
import           InternalIR.SimplePath
import           InternalIR.SSA
import           Utils.Pathsearch.Config
import           Utils.Pathsearch.ModuleInfo.Callsite
import           Prelude                   hiding (mod)

data PathSearchState = PathSearchState
      { cpsMod     :: ModuleInfo
      -- ^ Module we're currently in (thread local)
      , cpsFunc    :: Name
      -- ^ Function name we're in (thread local)
      , cpsExhaust :: Int
      -- ^ How many blocks have we explored? (thread local)
      , cpsWork    :: [Work]
      -- ^ Do we have any queued work to do? (thread local)
      , cpsCurPath :: SimplePath
      -- ^ Simple path (thread local)
      , cpsCfg     :: PathSearchCfg
      -- ^ Search config
      , cpsTGroup  :: ThreadGroup
      -- ^ Thread group
      , cpsProgInfo :: ProgramInfo
      -- Program info, that is, for all modules we're interested in (not just current module)
      , cpsQueue   :: TQueue SimplePath
      -- ^ The queue where all paths, when done are written to
      }

-- | Pathsearch monad is simply a statemonad (over IO)
type PS = StateT PathSearchState IO

-- | Get the current in-progress SimplePath this thread has traversed
getCurrentPath :: PS SimplePath
getCurrentPath = cpsCurPath <$> get

-- | Walk basic blocks, beginning with the one specified
walkBasicBlockFrom :: Name -> PS ()
walkBasicBlockFrom bbName = do
  -- Record that the basic block is being walked
  bb0 <- getBasicBlock bbName
  curFunc <- cpsFunc <$> get
  curMod <- cpsMod <$> get
  appendBB $ SimplePathBBEntry curMod curFunc bb0
  -- Check if the basic block contains any instructions we don't know how to handle and should terminate
  qPred <- (psQuitOn . cpsCfg) <$> get
  unless (shouldQuit qPred curMod bb0) $
    -- If we just entered a function, stitch up the formal-actual arguments
    maybeHandleCallArgs bb0 $ \bb1 ->
      -- If we're in a loop and we handle loops, handle it
      maybeHandleLoop bb1 $ \bb2 -> do
        -- If we handle calls and the basic block has functions we handle,
        -- inline them
        isCallBB <- handleCall bb2
        if isCallBB
          then walkCallBasicBlock bb2
          else walkNormalBasicBlock bb2

-- | Walk just the last portion of a basic block, starting from the given instruction number, then continue on.
-- Assumes that the "last portion" doesn't include phis, etc. You probably don't want this function, for most purposes you probably want walkBasicBlockFrom.
walkFromLastPortionOfBB :: BasicBlock -> Int -> PS ()
walkFromLastPortionOfBB bb instrNum = do
  -- compared to walkBasicBlockFrom:
  -- we've already done appendBB, in handleGenericReturn
  mInfo <- cpsMod <$> get
  qPred <- (psQuitOn . cpsCfg) <$> get
  unless (shouldQuit qPred mInfo bb) $ do  -- we quit based on the entire basic block. Sometimes we might quit even though the bad part of the basic block was above where we're starting. That's ok for now.
    -- also, we don't need the maybeHandleCallArgs step (there aren't any args to stitch up),
    -- but we might need the maybeHandleLoop step
    maybeHandleLoop bb $ \bb' -> do
      isCallBB <- handleCallFromN instrNum bb'
      if isCallBB
        then walkLastPortionOfCallBB bb' instrNum
        else walkLastPortionOfNormalBB bb' instrNum

-- | Do we need to handle a call in this basic block?
handleCall :: BasicBlock -> PS Bool
handleCall = handleCallFromN 0

-- | Do we need to handle a call in the [Nth to end] instructions of this basic block?
handleCallFromN :: Int -> BasicBlock -> PS Bool
handleCallFromN n bb = do cfg <- psEnterCalls . cpsCfg <$> get
                          curPath <- getCurrentPath
                          let names = getAllCalledFunctions $ drop n $ getBlockContents bb
                          return $ any (\f -> shouldHandleCall cfg f curPath) names

-- | Get the names of functions that are called in a set of instructions. We don't handle invoke for now.
-- If a function is inline assembly, it doesn't have a name, and we simply leave it out of the list.
getAllCalledFunctions :: [Named Instruction] -> [Name]
getAllCalledFunctions nis = mapMaybe getCalledFunctionName $ map withoutName nis
  where getCalledFunctionName i@(Call {}) = nameOfCalledFunc i
        getCalledFunctionName _           = Nothing

-- | Is the instruction a phi
isPhi :: Named Instruction -> Bool
isPhi (_ := Phi {}) = True
isPhi _             = False

-- | Basic block has phi nodes
hasPhi :: BasicBlock -> Bool
hasPhi bb = any isPhi $ getBlockContents bb

-- | Given basic block name, get the basic block in current function.
getBasicBlock :: Name -> PS BasicBlock
getBasicBlock bbName = do
  s0 <- get
  let fName = cpsFunc s0
      bBs   = fromJust $ bbsFromFunctionName (modAST $ cpsMod s0) fName  -- fromJust is OK because the current function must exist in the current module
  case filter (\bb -> getBlockName bb == bbName) $ bBs of
    [bb0] -> return bb0
    []    -> error $ "getBasicBlock: couldn't find basic block named " ++ show bbName ++ " in function " ++ show fName
    _     -> error $ "getBasicBlock: found multiple basic blocks named " ++ show bbName ++ " in function " ++ show fName

-- | Handle loop/repeated call block (if it is one); if we're in a loop but not
-- handling loops or calls this thread is done. Our loop detection is
-- imprecise. In particular if we start in the middle of a loop we won't detect
-- this. In practice, this doesn't really matter since want to enter loops.
maybeHandleLoop :: BasicBlock -> (BasicBlock -> PS ()) -> PS ()
maybeHandleLoop bb0 act = do
  s0 <- get
  let path = cpsCurPath s0
      idxs = SimplePathBBEntry (cpsMod s0) (cpsFunc s0) bb0 `elemIndices` bbs path
      inLoop = length idxs > 1
      handleLoops = psHandleLoops . cpsCfg $ s0
      handleCalls = dontEnterCalls /= (psEnterCalls . cpsCfg $ s0)
  -- not handling loops, but we're in a loop; die
  when (inLoop && not handleLoops && not handleCalls) $ die
  -- not in a loop, or we're handling loops; either way, continue
  act bb0

-- | Update exhaust, return True if we should continue
updateExhaust :: PS Bool
updateExhaust = do
  s0 <- get
  let exhaust = cpsExhaust s0 + 1
  put $ s0 { cpsExhaust = exhaust }
  return $ exhaust < (psPathLength $ cpsCfg s0)

-- | Are we out of exhaust and should stop?
isExhausted :: PS Bool
isExhausted = do
  s0 <- get
  return $ (cpsExhaust s0) > (psPathLength $ cpsCfg s0)

-- | Walk basic block that has calls in it.
walkCallBasicBlock :: BasicBlock -> PS ()
walkCallBasicBlock bb0 = do
  (sis1, bb1) <- processPhiNodesIfFirst bb0
  -- Convert basic blocks into simple instructions
  sis0 <- processNormalBasicBlockInstructions bb1
  let sis = sis0 ++ sis1
  -- Queue action to handle terminator
  enqueueWork $ WorkTerminator bb1
  -- Split the simple path into chunks of work to do:
  -- [non-call instructions], [call instruction], ...
  mapM_ enqueueWork $ simpleInstructionsToWork bb0 sis
  -- Start doing the actual work
  handleQueuedWork

-- | Walk last portion of a basic block that has calls in it.
-- Assumes no phis etc.
-- Again, you probably want walkCallBasicBlock.
walkLastPortionOfCallBB :: BasicBlock -> Int -> PS ()
walkLastPortionOfCallBB bb n = do
  enqueueWork $ WorkTerminator bb
  -- Split the simple path into chunks of work to do:
  -- [non-call instructions], [call instruction], ...
  mapM_ enqueueWork $ simpleInstructionsToWork bb $ map Instr $ drop n $ getBlockContents bb
  -- Start doing the actual work
  handleQueuedWork

-- | Process any existing work.
handleQueuedWork :: PS ()
handleQueuedWork = do
  exhausted <- isExhausted
  unless exhausted $ do
    mwrk <- dequeueWork
    case mwrk of
      Nothing -> return ()
      Just (WorkSimple sis)    -> appendInstrs sis >> handleQueuedWork
      Just (WorkTerminator bb) -> processBlockTerminator bb
      Just (WorkCall bb si)    -> processCall bb si
      Just wrk                 -> error $ "BUG: work " ++ show wrk ++ " is not handled by this function"

-- | Handle function call if we're configured to enter it and if we can find a
-- definition for it in any of our modules.
processCall :: BasicBlock -> SimpleInstruction -> PS ()
processCall bb (Instr ii) = do
  -- some things we'll need in order to decide whether to enter the call
  cfg <- (psEnterCalls . cpsCfg) <$> get
  let callInstr = withoutName ii
  curPath <- getCurrentPath
  -- action to do if we can't/shouldn't enter the call
  let dontEnterThisCall = appendInstrs [Instr ii] >> handleQueuedWork
  -- If we're supposed to enter, do so. Else leave the call uninterpreted.
  case nameOfCalledFunc callInstr of
    Nothing -> dontEnterThisCall  -- if it's inline assembly, leave uninterpreted
    Just newFuncName | not (shouldHandleCall cfg newFuncName curPath) -> dontEnterThisCall  -- configured to not enter this call
    Just newFuncName -> do
      curMod <- cpsMod <$> get
      progInfo <- cpsProgInfo <$> get
      let maybeFunc = case definedFunctionFromNameAndModule (modAST curMod) newFuncName of
                        Just func -> Just (curMod, func)  -- found in current module
                        Nothing   -> getFunctionInAnyModule progInfo newFuncName  -- search other loaded modules for the function
      case maybeFunc of
        Nothing -> dontEnterThisCall  -- function not defined anywhere we can find, so leave uninterpreted
        Just (newMod, newFunc) -> do
          -- even if we are configured to enter the function, and we can find
          -- the function definition, we still need to quit if we're out of
          -- basic blocks. but we defer this check until now because we don't want to inc the exhaust
          -- until we're sure that we will actually enter the function if exhaust is OK
          ok <- updateExhaust
          when ok $ do
            -- Append exit marker when done
            enqueueWork $ WorkSimple [ ExitCall $ getResultName ii ]
            -- Append enter before entering function
            appendInstrs [ EnterCall newFuncName ]

            -- Get the current function name
            curFuncName <- cpsFunc <$> get
            -- Enqueue work to set rval = return value of the function
            let spbe = SimplePathBBEntry curMod curFuncName bb
            case getResultName ii of
              Just rval -> enqueueWork $ WorkReturn spbe rval $ getFunctionRetType' newFunc
              Nothing   -> enqueueWork $ WorkVoidReturn spbe
            -- Enqueue work to set formal arg i = actual arg i
            let actualFuncArguments = arguments callInstr
            enqueueWork $ WorkArgs (map fst $ actualFuncArguments)

            -- Enter function

            -- Set the underlying function name to the new function name
            modify' $ \s -> s { cpsMod = newMod, cpsFunc = newFuncName }
            -- Walk the first basic block
            let newFuncBBs = fromJust $ bbsFromFunctionName (modAST newMod) newFuncName  -- fromJust is safe because we only have a Just if the function exists in that module
            walkBasicBlockFrom $ getBlockName $ head newFuncBBs
processCall _ i = error $ "processCall called on invalid instruction: " ++ show i

-- | Given the basic block of a newly entered function, prefix a few
-- instructions that set the formal and actual arguments equal to eachother.
maybeHandleCallArgs :: BasicBlock -> (BasicBlock -> PS ()) -> PS ()
maybeHandleCallArgs bb0@(BasicBlock bbName bbNIs bbT) act = do
  mwrk <- peekWork
  case mwrk of
    Just (WorkArgs ops) -> do
      -- Doing work now, remove from list
      void $ dequeueWork
      -- Get the formal function parameters and create instructions
      -- that set the formal = actual. Then, prefix these instructions to the
      -- basic block.
      modast <- modAST <$> cpsMod <$> get
      fName  <- cpsFunc <$> get
      let (params, varArgs) = fromJust $ paramsFromFunctionName modast fName  -- fromJust is safe because current function should always be in current module
          setEqPO (Parameter ty n _) op = setEq' n ty op []
          eqIns = zipWith setEqPO params ops
          bb1   = BasicBlock bbName (eqIns ++ bbNIs) bbT
      if varArgs
        then error "Variable arguments not yet supported"
        else act bb1
    _ -> act bb0

-- | This function handles return value stitching, for the case where we
-- previously pushed return work. i.e., it sets the return value of a function
-- to the value at the call site.
handleSpecificReturn :: Maybe Operand -> PS ()
handleSpecificReturn moper = do
  wrk <- dequeueWork
  spbe <- case wrk of
    -- we're in a call with return value
    Just (WorkReturn spbe nvar ty) -> do
      -- set the nvar = return value
      appendInstrs [setEq nvar ty oper []]
      return spbe
    Just (WorkVoidReturn spbe) -> return spbe
    _ -> error "BUG: expected call to have set up return work"
  -- restore the underlying module and function name
  modify' $ \s -> s { cpsMod = spbeMod spbe, cpsFunc = spbeFunc spbe }
  appendBB spbe
  -- done handling return, go back into basic block of callee
  handleQueuedWork
  where (Just oper) = moper

-- | This function handles return value stitching, for the case where we have
-- not previously pushed return work. That is, we encountered a return statement
-- for a function which we started inside of.
handleGenericReturn :: Maybe Operand -> PS ()
handleGenericReturn moper = do
  psState <- get
  let curFunc = cpsFunc psState
      callsites = getAllCallsites (cpsProgInfo psState) curFunc
  forM_ callsites $ \(callerMod, cs) -> fork $ do
    let callerFunc = getFunctionName' $ csFunc cs
    appendBB $ SimplePathBBEntry callerMod callerFunc (csBB cs)
    modify' $ \s -> s { cpsMod = callerMod, cpsFunc = callerFunc }
    let callInstr = (getBlockContents $ csBB cs) !! (csI cs)
    case getResultName callInstr of
      Nothing   -> return ()
      Just rval -> let rettype = fromJust $ functionRetTypeFromName (modAST callerMod) callerFunc  -- fromJust is safe because callerFunc must be in callerMod
                       (Just oper) = moper
                    in appendInstrs [setEq rval rettype oper []]
    walkFromLastPortionOfBB (csBB cs) (csI cs + 1)  -- add 1 so that we don't process the call itself
  maybeDie

-- | Work that needs to be done when walking a call basic block.
data Work = WorkSimple [SimpleInstruction] -- ^ Handle simple instructions
          | WorkCall BasicBlock SimpleInstruction -- ^ Handle call instruction
          | WorkTerminator BasicBlock      -- ^ Handle block terminator
          | WorkReturn SimplePathBBEntry Name Type -- ^ Handle function returns
          | WorkVoidReturn SimplePathBBEntry -- ^ Handle void-function returns
          | WorkArgs [Operand]             -- ^ Handle call argument stitching
          deriving (Eq, Show)

-- | Peek at the next work we need to do. This is useful for stitching return
-- values.
peekWork :: PS (Maybe Work)
peekWork = do
  mwrk <- cpsWork <$> get
  case mwrk of
    []    -> return Nothing
    wrk:_ -> return $ Just wrk

-- | Do we have any work to do?
haveWork :: PS Bool
haveWork = isJust <$> peekWork

-- | Pop the next work thing that needs to be handled, if any.
dequeueWork :: PS (Maybe Work)
dequeueWork = do
  s0 <- get
  case cpsWork s0 of
    []   -> return Nothing
    wrk:rest -> do put $ s0 { cpsWork = rest }
                   return $ Just wrk

-- | Queue work to do on behalf of a basic block.
enqueueWork :: Work -> PS ()
enqueueWork wrk = do
  s0 <- get
  put $ s0 { cpsWork = wrk : cpsWork s0 }

-- | Convert list of simple instructions to list of work that needs to be done.
simpleInstructionsToWork :: BasicBlock -> [SimpleInstruction] -> [Work]
simpleInstructionsToWork bb sis0 = reverse $ collapseSimple $ map simpleOrCall sis0
  where -- Convert an instruction into work.
        simpleOrCall i | (Just (Call {})) <- getInstr i = WorkCall bb i
        simpleOrCall i                                  = WorkSimple [i]
        -- Join any adjacent WorkSimple's into one.
        collapseSimple (WorkSimple i:WorkSimple j:rest) = collapseSimple $ WorkSimple (i++j) : rest
        collapseSimple (WorkSimple i:wrk:rest)          = WorkSimple i : wrk : collapseSimple rest
        collapseSimple (wrk:rest@(WorkSimple _:_))      = wrk : collapseSimple rest  -- cd wonders why this case only applies when 'rest' starts with a WorkSimple? Why not `collapseSimple (wrk:rest) = wrk : collapseSimple rest` here?
        collapseSimple wrk                              = wrk

-- | Walk normal basic block.
walkNormalBasicBlock :: BasicBlock -> PS ()
walkNormalBasicBlock bb0 = do
  (sis0, bb1) <- processPhiNodesIfFirst bb0
  sis1 <- processNormalBasicBlockInstructions bb1
  appendInstrs $ sis0 ++ sis1
  processBlockTerminator bb1

-- | Walk only the last portion of a basic block.
-- Assumes no phis etc.
-- Again, you probably want walkNormalBasicBlock instead, for most purposes.
walkLastPortionOfNormalBB :: BasicBlock -> Int -> PS ()
walkLastPortionOfNormalBB bb fromInstrNum = do
  appendInstrs $ map Instr $ drop fromInstrNum $ getBlockContents bb
  processBlockTerminator bb

-- | Handle terminator and rest of path
processBlockTerminator :: BasicBlock -> PS ()
processBlockTerminator bb = do
  ok <- updateExhaust
  if ok then case withoutName (getBlockTerminator bb) of
    Ret mop _                   -> do trackOps $ maybeToList mop
                                      specificReturn <- haveWork  -- if so, we know exactly where we're returning to
                                      if specificReturn then handleSpecificReturn mop
                                                        else do
                                                          followRets <- psFollowRets . cpsCfg <$> get
                                                          when followRets $ handleGenericReturn mop
    CondBr _ trueBr falseBr _   -> do fork $ processPathCondition bb trueBr >> walkBasicBlockFrom trueBr
                                      fork $ processPathCondition bb falseBr >> walkBasicBlockFrom falseBr
                                      die
    Br dst _                    -> walkBasicBlockFrom dst
    Switch _ defDst otherDsts _ -> do forM_ (defDst:(map snd otherDsts)) $ \dst -> fork $ do
                                         processPathCondition bb dst
                                         walkBasicBlockFrom dst
                                      die
    IndirectBr _ dsts _         -> do forM_ dsts $ \dst -> fork $ walkBasicBlockFrom dst
                                      die
    Unreachable{}               -> return ()
    CatchRet _ dst _            -> walkBasicBlockFrom dst
    term                        -> error $ "Terminator not yet supported: " ++ show term
  else  -- we don't need to continue to another block, but we do need to record the variables used in a track-ops
    trackOps $ getOperands $ getBlockTerminator bb

-- | Kill current thread
die :: PS ()
die = liftIO $ myThreadId >>= killThread

-- | Write the result to the TQueue. If we made it here, we're done.
writeResult :: PS ()
writeResult = do
  s0 <- get
  let SimplePath ins pairs = cpsCurPath s0
  -- Write the SSA'd path if it's not null
  unless (null ins) $
    -- TODO: we probably don't want to do this reversing so much
    liftIO $ atomically $ writeTQueue (cpsQueue s0) (toSSA $ SimplePath ins (reverse pairs))
  -- We're done, die
  die


-- | Erase thread if the thread has exhaust.
maybeDie :: PS ()
maybeDie = do
  s0 <- get
  when (cpsExhaust s0 < (psPathLength $ cpsCfg s0)) $ die

-- | We may start at a basic block where we have phis, turn the phis into
-- parallel instructions and rest of basic block.
processPhiNodesIfFirst :: BasicBlock -> PS ([SimpleInstruction], BasicBlock)
processPhiNodesIfFirst bb0@(BasicBlock bName sis0 bT) = do
  path <- getCurrentPath
  let shouldProcess = length (bbs path) == 1 && hasPhi bb0
  if shouldProcess
    then return (parInstrs, BasicBlock bName rest bT)
    else return ([], bb0)
  where (phis, rest) = span isPhi sis0
        parInstrs    = [phisToPar phis]


-- | Collapse basic blocks instructions into list of simple instructions. We
-- rewrite Phi nodes to setEq's.
processNormalBasicBlockInstructions :: BasicBlock -> PS [SimpleInstruction]
processNormalBasicBlockInstructions bb = do
  s0 <- get
  let path = cpsCurPath s0
      curFuncName = cpsFunc s0
      curMod = cpsMod s0
      -- filter basic blocks of other functions (ignore calls)
      -- TODO: this is actually not right, we need to worry about recursion
      myPrevBBs = filter (\spbe -> spbeMod spbe == curMod && spbeFunc spbe == curFuncName) $ bbs path
  let mPrev = case myPrevBBs of
                -- get previous block (ignoring any calls)
                (_:prevSPBE:_) -> Just $ getBlockName $ spbeBB prevSPBE
                _  -> Nothing
      sis0  = case mPrev of
                -- rewrite phi instructions to eqs
                Just prevBB -> map (rewriteInstr prevBB) $ getBlockContents bb
                -- prev function is not this function or not previous bb
                _ -> map Instr $ getBlockContents bb
  return sis0
  -- Append the basic block and list of simple instructions to this thread's
    where rewriteInstr prevBB phi@(nvar := Phi ty ivs meta) =
              let oper = case filter ((==prevBB) . snd) ivs of
                           [(o,_)] -> o
                           _ -> error $ unwords ["rewriteInstr: could not find", show prevBB, "in", show phi]
              in setEq nvar ty oper meta
          rewriteInstr _ ni = Instr ni

-- | Append instructions to simple path
appendInstrs :: [SimpleInstruction] -> PS ()
appendInstrs sis1 = do
  s0  <- get
  let SimplePath sis0 bb = cpsCurPath s0
  put $ s0 { cpsCurPath = SimplePath (sis0 ++ sis1) bb }

-- | Record the given SimplePathBBEntry in the SimplePath
appendBB :: SimplePathBBEntry -> PS ()
appendBB spbe = do
  s0 <- get
  let SimplePath is pairs = cpsCurPath s0
  put $ s0 { cpsCurPath = SimplePath is (spbe:pairs) }
  checkFilterPrefix

-- | Ensure this path is not one we want to filter out. If it is, then kill the thread.
checkFilterPrefix :: PS ()
checkFilterPrefix = do
  prefixes <- (psFilterPrefix . cpsCfg) `liftM` get
  unless (Set.null prefixes) $ do
    path <- getCurrentPath
    let pathBBs = reverse $ map asNames $ bbs path
                  -- TODO: reversing every time we call this function is inefficient, maybe
                  -- we could use a reversed version of prefix instead
        -- Crop the prefix path if it's longer than our currentPath
        crop prefix = take (length pathBBs) prefix
        -- Check if any prefix is a
        shouldCont = Set.any (\prefix -> crop prefix `isPrefixOf` pathBBs) prefixes
    unless shouldCont die

-- | Process path condition by setting branch condition according to the next
-- block's name.
processPathCondition :: BasicBlock  -- ^ Current basic block
                     -> Name  -- ^ Name of next block
                     -> PS ()
processPathCondition bb nextBlock = case getBlockTerminator bb of
   Do (CondBr op trueBr _ meta) -> appendInstrs [mkPathCond op (mkBool $ nextBlock == trueBr) meta]
   Do (Switch op _ otherDsts meta) ->
     case filter ((==nextBlock) . snd) otherDsts of
        [(c,_)] -> appendInstrs [mkPathCond op c meta] -- one of the destinations
        _ -> appendInstrs $ map (\(c, _) -> mkNotPathCond op c meta) otherDsts
   term -> error $ "processPathCondition: not yet supported: " ++ show term
  where mkPathCond op c meta = case op of
          LocalReference ty nvar -> setPathEq nvar ty (ConstantOperand c) meta
          _ -> error $ "mkPathCond: not yet supported: " ++ show op
        mkNotPathCond op c meta = case op of
          LocalReference ty nvar -> setPathNEq nvar ty (ConstantOperand c) meta
          _ -> error $ "mkNotPathCond: not yet supported: " ++ show op
        mkBool tf = C.Int 1 (if tf then 1 else 0)

-- | Track operands.
trackOps :: [Operand] -> PS ()
trackOps []  = return ()
trackOps ops = appendInstrs [TrackOps ops]

-- | Fork a thread wherein the path search computation will continue.
fork :: PS () -> PS ()
fork act = do
  s0 <- get
  void $ liftIO $ forkIO (cpsTGroup s0) $ void $ execStateT (act >> maybeDie >> writeResult) s0

-- | Get all simple paths of a particular length from module definition.
-- Simple paths don't contain branch instructions; everything is fully inlined
-- into the path. In general, you don't want to use this. This is a blocking
-- function and will be slow if the number of paths is huge. You probably
-- want 'forEachSimplePath'.
getAllSimplePaths :: PathSearchCfg   -- ^ Search config
                  -> Name            -- ^ Function to start in. We will find all paths starting at any BB in this function.
                  -> ModuleInfo      -- ^ LLVM module + precomputed module info. Must contain the given function.
                  -> [ModuleInfo]    -- ^ (Optional) Additional modules, which paths may be followed into
                  -> IO [SimplePath] -- ^ List of paths
getAllSimplePaths cfg fName mInfo xtraMods = do
  -- Get thread group and queue.
  (tg, queue) <- allSimplePaths cfg fName mInfo xtraMods
  -- Wait for all threads.
  wait tg
  -- Flush the queue and return unique paths
  paths <- atomically $ flushTQueue queue
  return $ nub paths


-- | Get a queue that will contain simple paths of a particular length from
-- module definition.  Simple paths don't contain branch instructions;
-- everything is fully inlined into the path.
allSimplePaths :: PathSearchCfg                       -- ^ Search config
               -> Name                                -- ^ Function to start in. We will find all paths starting at any BB in this function.
               -> ModuleInfo                          -- ^ LLVM module + precomputed module info. Must contain the given function.
               -> [ModuleInfo]                        -- ^ (Optional) Additional modules, which paths may be followed into
               -> IO (ThreadGroup, TQueue SimplePath) -- ^ Thread group and queue of paths
allSimplePaths cfg fName mInfo xtraMods = do
  queue <- newTQueueIO
  tg <- new
  let s0 = PathSearchState { cpsMod      = mInfo
                           , cpsFunc     = fName
                           , cpsExhaust  = 0
                           , cpsCfg      = cfg
                           , cpsWork     = []
                           , cpsTGroup   = tg
                           , cpsProgInfo = mInfo:xtraMods
                           , cpsQueue    = queue
                           , cpsCurPath  = emptySimplePath
                           }
  -- Fork a thread for each function basic block and start walking each block
  flip evalStateT s0 $ forM_ filtBBs $ \bb -> fork $ walkBasicBlockFrom $ getBlockName bb
  -- Return queue to consumers
  return (tg, queue)
    where funcBBs = fromJust $ bbsFromFunctionName (modAST mInfo) fName  -- fromJust is OK because we require caller to ensure that the provided function exists in the provided module
          -- Filter out any of the function basic blocks that don't form a
          -- valid prefix
          filtBBs = let prefixes = psFilterPrefix cfg
                        headOrDie prefix = case prefix of
                                            (x:_) -> x
                                            _     -> error "BUG: path prefix should not be the empty list"
                        validBBs = Set.map headOrDie prefixes
                        isValidPrefix bb = (modName mInfo, fName, getBlockName bb) `Set.member` validBBs
                    in if Set.null prefixes
                         then funcBBs
                         else filter isValidPrefix funcBBs


-- | Like 'allSimplePaths', but execute an action per unique simple path. To
-- make progress, the action should execute in a separate thread.
forEachSimplePath :: PathSearchCfg                       -- ^ Search config
                  -> Name                                -- ^ Function to start in. We will find all paths starting at any BB in this function.
                  -> ModuleInfo                          -- ^ LLVM module + precomputed module info
                  -> [ModuleInfo]                        -- ^ (Optional) Additional modules, which paths may be followed into
                  -> (SimplePath -> IO ())               -- ^ Action to execute per path
                  -> IO ()
forEachSimplePath cfg fName mInfo xtraMods act = do
  (tGroup, tQueue) <- allSimplePaths cfg fName mInfo xtraMods
  -- Create MVar to hold all the the paths we've passed on to the action
  -- already and thus only handle unique paths.
  allPaths <- newMVar Set.empty
  untilDone allPaths tGroup tQueue act

-- | Helper function for 'forEachSimplePath' that runs the action until completion
untilDone :: MVar (Set SimplePath) -> ThreadGroup -> TQueue SimplePath -> (SimplePath -> IO ()) -> IO ()
untilDone allPaths tGroup tQueue act = do
  -- Get one or all remaining paths
  (isDone, ps0) <- atomically $ getNextPath
  -- Add any of the new paths to allPaths and get the new unique paths
  ps1 <- modifyMVarMasked allPaths $ \paths ->
           return (Set.union paths ps0, ps0 Set.\\ paths)
  -- For each new unique path execute the action
  forM_ (Set.toList ps1) act
  -- If we're not done (we may be at this point because other threads may have
  -- consumed more), keep trying
  unless isDone $ untilDone allPaths tGroup tQueue act

  where getNextPath = do
          done <- (==0) <$> nrOfRunning tGroup
          ps <- if done
                  then Set.fromList <$> flushTQueue tQueue
                  else do mp <- tryReadTQueue tQueue
                          case mp of
                            Nothing -> retry
                            Just  p -> return $ Set.singleton p
          return (done, ps)
