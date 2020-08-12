-- | Checker configuration definition
module Static.CheckerConfigDef where
import           Data.ByteString       (ByteString)
import qualified Data.Map              as M
import qualified Data.Set              as S
import           InternalIR.ModuleInfo
import           LLVM.AST
import           LLVMAST.Interface
import           Static.CheckerState

-- | User-defined static checker configuration
data CheckerConfig a b = CheckerConfig {
      cfgShouldCheckFile     :: ByteString -> Bool,
                             -- ^ Check the file?
      cfgShouldCheckModule   :: Module -> Bool,
                             -- ^ Check the module?
      cfgShouldCheckFunction :: Definition -> Bool,
                             -- ^ Check the function?
      cfgVerbose             :: Bool,
                             -- ^ Verbose output?
      cfgBlockBound          :: Int,
                             -- ^ How many blocks should we explore
      cfgLoopBound           :: Int,
                             -- ^ How many times through loops?
      cfgDebugPath           :: Maybe Path,
                             -- ^ Send it down this path
      cfgAccumState          :: Bool,
                             -- ^ Should we accumulate the state or reset it?
      cfgStartState          :: a,
                             -- ^ User-provided start state
      cfgCheck               :: Int -> Named Instruction -> Checker a b (),
                             -- ^ User-provided static checking function
      cfgInitialAction       :: Checker a b (),
                             -- ^ Action to preform before starting to check
      cfgFinalAction         :: Checker a b (),
                             -- ^ Action to perform after finishing checking
      cfgDieOnBug            :: Bool,
                             -- ^ Stop checking once you've found a bug?
      cfgGetStarts           :: M.Map Name BasicBlock -> [BasicBlock] -> [Name],
                             -- ^ Function that returns all blocks from which
                             -- to start checking
      cfgGoBackBy            :: Int
                             -- ^ Should we start some number of blocks before
                             -- the user-provided start? This is just a convience,
                             -- users can obviously replicate this in cfgGetStarts
    }

stateFromConfig :: FilePath            -- ^ Path to the file
                -> ModuleInfo          -- ^ The module info
                -> Definition          -- ^ The function definition
                -> CheckerConfig a b   -- ^ User's static checker config
                -> [CheckerState a b]  -- ^ The initialized checker state
stateFromConfig filepath mod' defn config =
  map (\start -> CheckerState { blockBound = cfgBlockBound config
                              , loopBound = cfgLoopBound config
                              , debugPath = cfgDebugPath config
                              , allBlocks = blocks
                              , blockList = bbs
                              , curName = funName
                              , curParams = funParams
                              , curTypes = modProgramTypes mod'
                              , curAliases = M.empty
                              , curOffsets = M.empty
                              , curMemory = M.empty
                              , curFilepath = filepath
                              , curPath = [start]
                              , curBlock = start
                              , curState = cfgStartState config
                              , curBugs =  error "PLEASE INITIALIZE TQUEUE"
                              , curTGroup = error "PLEASE INITIALIZE THREAD GROUP"
                              , shouldDieOnBug = cfgDieOnBug config
                              }) starts
  where
    -- We have already checked that this definition is a function and that
    -- the function contains basic blocks at the point where stateFromConfig
    -- is called, so the following is safe.
    bbs = getFunctionBBs' defn
    blocks = M.fromList $ map (\b -> (getBlockName b, b)) bbs
    starts = goBack blocks (cfgGoBackBy config) $ cfgGetStarts config blocks bbs
    funName = getFunctionName' defn
    getParamInfo (Parameter ty name _) = (name, ty)
    funParams = M.fromList $ map getParamInfo $ fst $ getFunctionParams' defn

goBack :: M.Map Name BasicBlock
       -> Int
       -> [Name]
       -> [Name]
goBack blockMap n starts = getPredsOf (getPredBlocks blockMap) starts n

getPredsOf :: M.Map Name (S.Set Name) -> [Name] -> Int -> [Name]
getPredsOf _ blocks 0         = blocks
getPredsOf preds blocks bound =
  let newBlocks = map (\block -> M.findWithDefault S.empty block preds) blocks
  in getPredsOf preds (concatMap S.toList newBlocks) (bound - 1)

getPredBlocks :: M.Map Name BasicBlock -> M.Map Name (S.Set Name)
getPredBlocks bbs =
  foldr (\(name, bb) curMap ->
    let nextBlocks = getNextBlocksOf bb
        preds = map (\nextBlock -> (nextBlock, S.fromList [name])) nextBlocks
    in M.unionWith S.union (M.fromList preds) curMap
  ) M.empty $ M.toList bbs
