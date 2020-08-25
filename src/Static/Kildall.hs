module Static.Kildall where
import           Control.Monad       (forM)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           LLVM.AST            hiding (Store)
import           LLVMAST.Interface
import           Prelude             hiding (elem)
import           Static.CheckerState

type NodeId = (Name, Int)

data WorkNode a = WorkNode { workNode  :: NodeId
                           , nodeState :: a
                           }
                deriving (Eq, Ord, Show)

nodeBlock :: WorkNode a -> Name
nodeBlock = fst . workNode

nodeLine :: WorkNode a -> Int
nodeLine = snd . workNode

data Store a = Store { storeMap :: Map NodeId a }

getSuccessors :: (Show a) => WorkNode a -> Checker b c [NodeId]
getSuccessors node = do
  blocks <- getBlocks
  let block = case M.lookup (nodeBlock node) blocks of
                   Just foundBlock -> foundBlock
                   Nothing         -> error $ unwords $ ["Could not find block for", show node]

      line = nodeLine node
  if line == length (getBlockContents block) - 1
  then return $ makeNextBlockNodes $ case getBlockTerminator' block of
         Ret{}              -> []
         CondBr _ br1 br2 _ -> [br1, br2]
         Br next _          -> [next]
         Switch _ def brs _ -> def:map snd brs
         IndirectBr _ brs _ -> brs
         _                  -> error $ unwords ["Unexpected terminator in", show block]
  else return [(nodeBlock node, line + 1)]
  where makeNextBlockNodes = map $ \b -> (b, 0)

infoAt :: (Show a) => WorkNode a -> Store a -> a
infoAt node (Store store) = case M.lookup (workNode node) store of
                              Just info -> info
                              Nothing -> error $ unwords ["Could not find info in store for"
                                                         , show $ workNode node
                                                         ]

updateStore :: WorkNode a -> a -> Store a -> Store a
updateStore node item (Store store) = Store $ M.insert (workNode node) item store

-- | We have to be able to combine two peices of analysis information
-- in order to check the program using Kilall's algorithm
-- We also need a transfer function that "propagates information thru an expression"
class Checkable a where
    meet :: a -> a -> Checker b c a
    transfer :: WorkNode a -> Checker b c (WorkNode a)

-- | http://www.ccs.neu.edu/home/types/resources/notes/kildall/kildall.pdf
-- Kildall's algorithm for computing program information to a fixed point
kildall :: (Checkable a, Eq a, Show a)
        => [WorkNode a] -- ^ Worklist
        -> Store a -- ^ Store of analysis information
        -> Checker b c (Store a)
kildall [] store = return store
kildall (elem:rest) store = do
  let incomingState = nodeState elem
      currentState = infoAt elem store
  newState <- meet incomingState currentState
  if newState == currentState
  then kildall rest store
  else do
    succs <- getSuccessors elem
    let newStore = updateStore elem newState store
    transferredState <- transfer $ WorkNode (workNode elem) newState
    next <- forM succs $ \e -> return $ WorkNode e (nodeState transferredState)
    kildall (rest++next) newStore


