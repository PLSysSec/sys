{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

This module includes the checker monad and exports all functions that
users use to interact with that monad: adding bugs, get their user defined state,
get the path to the current instruction, etc.

First, it includes the CheckerState definition, monad definition, and standard
state monad-style functions. Then, it provides getters and setters that allow
users to update the state. Third, it implements the functions that construction
the LLVM IR CFG on the fly. Finally, it provides helper functions that make it
easier for users to write their own checkers.

-}
module Static.CheckerState ( initState
                           , getNextBlocks
                           , getPath
                           , getBlocks
                           , getBlockList
                           , getFilepath
                           , getFunName
                           , getParamNames
                           , getParams
                           , getNamedType
                           , addBug
                           , getCurBlock
                           , getBlockBound
                           , getLoopBound
                           , unChecker
                           , advanceTo
                           , isFinalInstr
                           , resetPath
                           , getState
                           , putState
                           , blankState
                           , getBugs
                           , die
                           , Checker
                           , CheckerState(..)
                           , evalChecker
                           , runChecker
                           , execChecker
                           , Path
                           , storeTo
                           , getFrom
                           , allocVar
                           , addAlias
                           , getAliasKey
                           , operandIsAlias
                           , nameIsAlias
                           , getAliasesOf
                           ) where
import           Control.Concurrent              (killThread, myThreadId)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Thread.Group
import           Control.Monad.State.Strict      hiding (state)
import           Control.Monad.STM               (atomically)
import qualified Data.Map                        as M
import           Data.Maybe                      (isJust)
import qualified Data.Set                        as S
import           Data.Tuple                      (swap)
import           InternalIR.SSA                  (toSSA)
import           LLVM.AST
import qualified LLVM.AST.Operand                as O
import           LLVMAST.Interface               hiding (getFunName)

-- | The checker monad's internal state
data CheckerState a b = CheckerState {
  blockBound     :: Int,                     -- ^ When to stop
  loopBound      :: Int,                     -- ^ How many times through a loop?
  debugPath      :: Maybe Path,              -- ^ The path to re-run if in debug mode
  allBlocks      :: M.Map Name BasicBlock,   -- ^ The function
  blockList      :: [BasicBlock],            -- ^ A list of blocks in the function
  curName        :: Name,                    -- ^ The name of the function
  curParams      :: M.Map Name Type,         -- ^ The arguments to the function
  curTypes       :: M.Map Name Type,         -- ^ All the named types in the function
  curAliases     :: M.Map Name (S.Set Name), -- ^ Aliased variables
  curOffsets     :: M.Map Name (S.Set Name), -- ^ Offset variables
  curMemory      :: M.Map Name Operand,      -- ^ Approximate state of memory
  curFilepath    :: FilePath,                -- ^ The filepath
  curPath        :: Path,                    -- ^ Current path
  curBlock       :: Name,                    -- ^ The current block
  curState       :: a,                       -- ^ Attacker-defined state
  curBugs        :: TQueue b,                -- ^ Bugs
  curTGroup      :: ThreadGroup,             -- ^ Thread group
  shouldDieOnBug :: Bool                     -- ^ Should we kill the thred if bug found?
  }

newtype Checker a b c = Checker { unChecker :: StateT (CheckerState a b) IO c }
  deriving (Functor, Applicative, Monad, MonadState (CheckerState a b), MonadIO)

blankState :: CheckerState a b
blankState = undefined

initState :: CheckerState a b
          -> Checker a b ()
initState state = put state

evalChecker :: Checker a b c -> CheckerState a b -> IO c
evalChecker (Checker sact) = evalStateT sact

runChecker :: Checker a b c -> CheckerState a b -> IO (c, CheckerState a b)
runChecker (Checker sact) = runStateT sact

execChecker :: Checker a b c -> CheckerState a b -> IO (CheckerState a b)
execChecker (Checker sact) = execStateT sact

---
--- Internal state getter and setters
---

-- | Get all the blocks in the current function
getBlocks :: Checker a b (M.Map Name BasicBlock)
getBlocks = allBlocks `liftM` get

-- | Get the blocks in the current function in list form
getBlockList :: Checker a b [BasicBlock]
getBlockList = blockList `liftM` get

-- | Get the user-defined state
getState :: Checker a b a
getState = curState `liftM` get

-- | Set the user-defined state
putState :: a -> Checker a b ()
putState state = do
  s0 <- get
  put $ s0 { curState = state }

-- | Get the user-provided path length (e.g., the user said to check
-- paths of only length 20 or less)
getBlockBound :: Checker a b Int
getBlockBound = blockBound `liftM` get

-- | Get the user-provided loop bound (e.g., the user said to execute loops a
-- maximum of 20 times)
getLoopBound :: Checker a b Int
getLoopBound = loopBound `liftM` get

-- | Get the debug path set by the user. For example, if the user is trying to
-- determine the cause of a false positive on path [1,2,4], they may set the debug
-- path to [1,2,4] in order to reproduce the failure.
getDebugPath :: Checker a b (Maybe Path)
getDebugPath = debugPath `liftM` get

-- | Set the debug path. See above.
setDebugPath :: Path -> Checker a b ()
setDebugPath path = do
  s0 <- get
  put $ s0 { debugPath = Just path }

-- | Get all the blocks in the current path in order
getPathBlocks' :: CheckerState a b -> [BasicBlock]
getPathBlocks' state =
  let path = curPath state
      bMap = allBlocks state
  in map (\bName -> bMap M.! bName)  path

-- | Get the current path
getPath :: Checker a b [Name]
getPath = curPath `liftM` get

-- | Get the current function
getFunName :: Checker a b Name
getFunName = curName `liftM` get

-- | Get the names of the parameters to the current function
getParamNames :: Checker a b [Name]
getParamNames = M.keys `liftM` curParams `liftM` get

-- | Get a map of the parameters to the current function and their types
getParams :: Checker a b (M.Map Name Type)
getParams = curParams `liftM` get

-- | Get the underlying type associated with a given type name
getNamedType :: Name -> Checker a b (Maybe Type)
getNamedType name = do
  types <- curTypes `liftM` get
  return $ M.lookup name types

-- | Get the filepath to the current file
getFilepath :: Checker a b FilePath
getFilepath = curFilepath `liftM` get

-- | Reset the current path to be replaced by the given path
resetPath :: [Name] -> Checker a b ()
resetPath path = do
  s0 <- get
  put $ s0 { curPath = path }

-- | Add a bug to the list of bugs that the checker has found
addBug :: (Ord b) => b -> Checker a b ()
addBug bug = do
  s0 <- get
  tQueue <- curBugs <$> get
  liftIO $ atomically $ writeTQueue tQueue bug
  when (shouldDieOnBug s0) die

-- | Get the list of bugs that the checker has found
getBugs :: (Show b, Ord b) => Checker a b (S.Set b)
getBugs = do
  tg <- curTGroup <$> get
  tQueue <- curBugs <$> get
  bugs <- liftIO $ do
    wait tg
    atomically $ flushTQueue tQueue
  return $ S.fromList bugs

-- | Kill current thread
die :: Checker a b ()
die = liftIO $ myThreadId >>= killThread

---
--- On the fly CFG construction
---

-- | Get the current block on the path
getCurBlock :: Checker a b BasicBlock
getCurBlock = getCurBlock' `liftM` get

-- | The internals of getting the current block on the path.
-- This does some of the construction of the control flow graph
-- (e.g., it resolves phi nodes in the current block so that their
-- LHS is assigned to the correct RHS)
getCurBlock' :: CheckerState a b -> BasicBlock
getCurBlock' state =
  let blockName = curBlock state
      blocks = allBlocks state
      path = curPath state
  in case M.lookup blockName blocks of
    -- We can't/shouldn't have to resolve phis if the we haven't been to two blocks yet
    Just block | length path < 2 -> block
    -- Otherwise, we need to resolve the phis
    Just (BasicBlock name instrs term) ->
      let prevBlock = path !! (length path - 2)
          newInstrs = map (\ninstr ->
            case ninstr of
              (name' := Phi ty vals m) ->
                  case M.lookup prevBlock $ M.fromList $ map swap vals of
                    -- We found the previous block in the list of phis, so replace
                    -- the phi node with an assignment to the correct value.
                    Just op -> name' := BitCast op ty m
                    -- We didn't find the phi node in a previous block---this shouldn't
                    -- happen, since we don't try to resolve phis until we've been to
                    -- at least two blocks
                    _       -> error "Could not resolve phi node"
              _ -> ninstr) instrs
      in last $ toSSA $ (init $ getPathBlocks' state) ++ [BasicBlock name newInstrs term]
    _ -> error $ unwords ["Could not find block"
                         , show blockName
                         , "in"
                         , show blocks
                         ]

getNextBlocks :: Checker a b [Name]
getNextBlocks = do
  path <- getDebugPath
  nextBlocks <- getNextBlocks'
  case path of
    -- We are just running on one specific path
    Just (next:rest) -> do
      if next `elem` nextBlocks
      then setDebugPath rest >> return [next]
      else return []
    Just [] -> return []
    -- Not running a specific path, just get the next blocks
    _ -> return nextBlocks

getNextBlocks' :: Checker a b [Name]
getNextBlocks' = do
  block <- getCurBlock
  return $ case getBlockTerminator' block of
             Ret{}              -> []
             CondBr _ br1 br2 _ -> [br1, br2]
             Br next _          -> [next]
             Switch _ def brs _ -> def:map snd brs
             IndirectBr _ brs _ -> brs
             -- We don't recognize the terminator, so stop exploring
             _                  -> []

advanceTo :: Name -> Checker a b ()
advanceTo next = do
  s0 <- get
  put $ s0 { curBlock = next
           , curPath = curPath s0 ++ [next]
           }

isFinalInstr :: Int -> Checker a b Bool
isFinalInstr instrIndex = do
  block <- getCurBlock
  let term = getBlockTerminator' block
  return $ if isRet term
  then let blockLength = length $ getBlockContents block
       in instrIndex == blockLength - 1
  else False

--
-- Static checker writing helper functions
--

-- | Simple store
storeTo :: Operand -- ^ Address
        -> Operand -- ^ Value
        -> Checker a b ()
storeTo (O.LocalReference _ addrName) val = do
  s0 <- get
  put $ s0 { curMemory = M.insert addrName val $ curMemory s0 }
storeTo _ _ = return ()

-- | Simple load
getFrom :: Operand -- ^ Address
        -> Checker a b (Maybe Operand) -- ^ Value if it exists
getFrom (O.LocalReference _ addrName) = do
  s0 <- get
  return $ M.lookup addrName $ curMemory s0
getFrom _ = return Nothing

-- | Simple allocation
allocVar :: Name -> Checker a b ()
allocVar name = do
  s0 <- get
  put $ s0 { curAliases = M.insert name (S.fromList [name]) $ curAliases s0 }

-- | Assignment
addAlias :: Name -> Operand -> Checker a b ()
addAlias newName (O.LocalReference _ oldName) = do
  s0 <- get
  aliasKey <- getAliasKey oldName
  let oldAliases = curAliases s0
  case aliasKey of
    Just key -> do
      let newAliases = M.adjust (S.insert newName) key oldAliases
      put $ s0 { curAliases = newAliases }
    Nothing -> do
      put $ s0 { curAliases = M.insert newName (S.fromList [newName, oldName]) oldAliases }
addAlias _ _                                  = return ()

-- | Does the operand alias the other operand?
operandIsAlias :: Operand -> Operand -> Checker a b Bool
operandIsAlias (O.LocalReference _ name) op = nameIsAlias name op
operandIsAlias _ _                          = return False

-- | Does the name alias the operand?
nameIsAlias :: Name -> Operand -> Checker a b Bool
nameIsAlias name (O.LocalReference _ otherName) = do
  ak1 <- getAliasKey name
  ak2 <- getAliasKey otherName
  return $ isJust ak1 && ak1 == ak2
nameIsAlias _ _ = return False

-- | Get all aliases of the name
getAliasesOf :: Name -> Checker a b (S.Set Name)
getAliasesOf name = do
  key <- getAliasKey name
  case key of
    Nothing -> return $ S.fromList [name]
    Just k -> do
      aliases <- curAliases `liftM` get
      return $ aliases M.! k

-- | Hidden function: how we do alias tracking
-- We give every alias set a 'key.' This is limited though:
-- it doesn't handle path constraints right now!
getAliasKey :: Name -> Checker a b (Maybe Name)
getAliasKey name = do
  aliases <- curAliases `liftM` get
  let keys = M.filter (S.member name) aliases
  case M.keys keys of
    -- This name has no aliases
    []    -> return Nothing
    -- This is the key into the alias map at which you will find this name's aliases
    [key] -> return $ Just key
    -- This should never happen: the point of the alias key is to only ever have
    -- a single key to all a variables aliases in the map
    _     -> error $ unwords ["Should not find multiple alias keys for", show name]

