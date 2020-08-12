{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Symex.Symex.SymexState (
                              -- * Solver result getter
                                getSolverResult
                              -- * Gatting and setting attacker state
                              , getAttackerState
                              , setAttackerState
                              -- * Making new variables and querying about variables
                              , getPointerSize
                              , VarName
                              , getAllTrackedVariables
                              , freshName
                              , number
                              , wnumber
                              , variableIsTracked
                              , variableExistsInProgram
                              -- * Memory versions
                              , currentMemoryVersion
                              , nextMemoryVersion
                              , currentShadowVersion
                              , nextShadowVersion
                              , useShadowMem
                              -- * Adding constraints
                              , addConstraint
                              -- * Operations on types
                              , getType
                              , getNamedType
                              -- * Allocation, pointers, and sizes
                              , strictAllocationOn
                              , oobsOk
                              , maxJumpSize
                              -- * Queries about program types and constants
                              , getUnnamedTy
                              , numElems
                              , elemTypes
                              , isPointerTy
                              , isArrayTy
                              , isAggregateTy
                              , isVectorTy
                              , referentTy
                              , baseType
                              -- * Path info
                              , getSymexPathInfo
                              -- * Symex state and monad operations
                              , blankState
                              , getResult
                              , doSymexAction
                              , Symex
                              , SymexState(..)
                              -- * Solving and solver results
                              , solve
                              , SolverResult(..)
                              , solverSat
                              , solverUnsat
                              , solverFailed
                              -- * Debugging (some of which is going away)
                              , printSymex
                              , printBoolector
                              , showBoolector
                              , dumpConstraints
                              ) where

import qualified Control.Monad.Fail         as Fail
import           Control.Monad.State.Strict
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Word                  (Word32, Word64)
import           InternalIR.PathInfo
import           LLVM.AST.AddrSpace
import           LLVM.AST.DataLayout
import           LLVM.AST.Name
import           LLVM.AST.Type              hiding (void)
import           Prelude                    hiding (elem)
import qualified Symex.Symex.Boolector      as B
import           Symex.Symex.Utils


-- | Recordkeeping for the symbolic execution.
-- Tracks the current list of constraints, variable types, active variables, etc
data SymexState a = SymexState {
  attackerState       :: a
  -- | The variables in the program or program snippet that we're executing
  , vars              :: Map VarName B.Node
  -- | The types of the variables in the program
  , tys               :: Map VarName Type
  -- | The sizes of types in the program
  , sizes             :: Map Type Word32
  -- | The offsets of indecies into types in program
  , offsets           :: Map Type [Word64]
  -- | All of the constraints on the existing variables
  , constraints       :: [B.Node]
  -- | All of the assignments to existing registers
  , assigns           :: M.Map VarName (S.Set B.Node)
  -- | All the assignments to existing shadow registers
  , shadowAssigns     :: M.Map VarName (S.Set B.Node)
  -- | A list of the different memory versions. Each time we store to memory,
  -- we add a new memory version that corresponds to the updated memory
  , memVersions       :: [B.Node]
  -- | A list of the different shadow memory versions.
  -- Same as normal memory but stores metadata using your choice of bitpattern
  , shadowMemVersions :: [B.Node]
  -- | Turn shadow memory on or off
  , shadowMem         :: Bool
  -- | Should we allow OOBs access?
  , oobAllowed        :: Bool
  -- | We support a bunch of different memory layouts
  , memLayout         :: DataLayout
  -- | The size of pointers, as determined from the data layout
  , pointerSize       :: Word32
    -- | The size of memory cells, as determined from the data layout
  , cellSize          :: Word32
  -- | Concrete allocations!
  , curPointer        :: Word32
  -- | Cache of things we've already allocated so we don't have to keep checking
  , alreadyAllocd     :: Set B.Node
  -- | Arguments and calls can alias
  , canAlias          :: Bool
  -- | Allocate everything a certain strict address? Or not!
  , strictAllocation  :: Bool
  -- | Counter for fresh variables so that users can create their own names
  -- that don't conflict with any existing ones
  , freshCtr          :: Int
  -- | Type aliases etc
  , pathInfo          :: PathInfo
  -- | Timeout after solverTimeout seconds
  , solverTimeout     :: Integer
  -- | The result of calling solve with the current symex state
  , solverResult      :: SolverResult
}

newtype Symex a b = Symex (StateT (SymexState a) B.Boolector b)
  deriving (Functor, Applicative, Monad, MonadState (SymexState a), MonadIO)

instance B.MonadBoolector (Symex a) where
  getBoolectorState = Symex $ lift $ get
  putBoolectorState state = Symex $ lift $ put state

instance Fail.MonadFail (Symex a) where
  fail = Fail.fail

--
-- Simple queries
--

-- | The size of pointers.
-- This is set based on the layout information at the top of the LLVM file.
-- If there is no layout information provided, it is set to a default of
-- 32-bit pointers in the blank symex state (blankState)
getPointerSize :: Symex a Word32
getPointerSize = pointerSize `liftM` get

-- | Is the symbolic execution engine assigning pointers locations?
-- There are two options here:
-- 1. Yes, all freshly allocated pointers have a concrete location or location constraint
-- 2. No. Pointers may alias arbitrarily.
-- You probably want option 1. Option 2 is enough for some checkers, though.
strictAllocationOn :: Symex a Bool
strictAllocationOn = strictAllocation `liftM` get

-- | Are oobs ok?
oobsOk :: Symex a Bool
oobsOk = oobAllowed `liftM` get

-- | Get the most recent solver result (monadic)
getSolverResult :: Symex a SolverResult
getSolverResult = solverResult `liftM` get

-- | Get the solver result from the symex state struct (pure)
getResult :: SymexState a -> SolverResult
getResult = solverResult

--
-- Attacker state
--

-- | Get the user-defined attacker state
getAttackerState :: Symex a a
getAttackerState = attackerState `liftM` get

-- | Set the user-defined attacker state
setAttackerState :: a -> Symex a ()
setAttackerState state = do
  s0 <- get
  put $ s0 { attackerState = state }

--
-- Variables
--

-- | Get all the variables whose values we are currently tracking.
-- If the symex state is tracking a variable, it knows both
-- (1) its symbolic value and (2) its type
getAllTrackedVariables :: Symex a (Map VarName B.Node)
getAllTrackedVariables = vars `liftM` get

-- | Does the symbolic state have a record of this variable in its
-- map of (VarName, Symbolic value) pairings?
variableIsTracked :: VarName -> Symex a Bool
variableIsTracked varName = do
  allVars <- vars `liftM` get
  return $ M.member varName allVars

-- | Does the variable exist somewhere in the path we are symbolically executing?
variableExistsInProgram :: VarName -> Symex a Bool
variableExistsInProgram varName = do
  programVars <- variableTypes `liftM` pathInfo `liftM` get
  return $ M.member varName programVars

--
-- Types
--

-- | Get the type of a variable (name)
getType :: VarName -> Symex a Type
getType varName = do
  s0 <- get
  let allTys = tys s0
  case M.lookup varName allTys of
    Just ty -> return ty
    Nothing -> error "Cannot get type of nonexistent variable"

-- | Get the type associated with a named type reference
getNamedType :: Name -> Symex a Type
getNamedType typeName = do
  allNamedTypes <- programTypes `liftM` pathInfo `liftM` get
  case M.lookup typeName allNamedTypes of
    -- It's not an opaque type so we have the size information for it
    Just ty -> return ty
    -- It's an opaque type: we will never be using anything other than a pointer to
    -- this type, so it's safe to just return a dummy type
    _       -> return $ IntegerType 32

--
-- Loads and stores
--

-- | Get a fresh version of memory (e.g. to store to)
nextMemoryVersion :: Symex a B.Node
nextMemoryVersion = do
  s0 <- get
  let memVers = memVersions s0
      memVerName = memName ++ show (length memVers)
      pointerWidth = fromIntegral $ pointerSize s0
      cellWidth = fromIntegral $ cellSize s0
  memVar <- B.makeMemory memVerName pointerWidth cellWidth
  put $ s0 { memVersions = memVar:memVers }
  return memVar

-- | Get a fresh version of shadow memory
nextShadowVersion :: Symex a B.Node
nextShadowVersion = do
  s0 <- get
  let shadowVers = shadowMemVersions s0
      shadowVerName = shadowMemName ++ show (length shadowVers)
      pointerWidth = fromIntegral $ pointerSize s0
      cellWidth = fromIntegral $ cellSize s0
  shadowVar <- B.makeMemory shadowVerName pointerWidth cellWidth
  put $ s0 { shadowMemVersions = shadowVar:shadowVers }
  return shadowVar

-- | Get the current memory version (e.g., to load from)
currentMemoryVersion :: Symex a B.Node
currentMemoryVersion = do
  s0 <- get
  return $ head $ memVersions s0

-- | Get the current version of shadow memory
currentShadowVersion :: Symex a B.Node
currentShadowVersion = do
  s0 <- get
  return $ head $ shadowMemVersions s0

-- | How we refer to memory
memName :: String
memName = "__global_mem__"

-- | How we refer to shadow memory
shadowMemName :: String
shadowMemName = "__shadow_mem__"

-- | Should we be using shadow memory at all?
useShadowMem :: Symex a Bool
useShadowMem = shadowMem `liftM` get

--
-- Operations on constraints
--

addConstraint :: B.Node -> Symex a ()
addConstraint constr = do
  s0 <- get
  put $ s0 { constraints = constr:(constraints s0) }

-- | The maximum size of any GEP jump. This and the heapAlloationBound
-- conspire to ensure that no combination of jumps allows us to
-- smash into some other peice of already-heap-allocated memory.
maxJumpSize :: Symex a Word32
maxJumpSize = fromIntegral `liftM` indexBound `liftM` pathInfo `liftM` get

--
-- New unique variables
--

-- | A new unique name
freshName :: Symex a VarName
freshName = do
  s0 <- get
  -- Increment the counter
  let freshNum = freshCtr s0
  put $ s0 { freshCtr = freshNum + 1}
  -- Return the new name
  return $ "__fresh__" ++ show freshNum


-- Path info

getSymexPathInfo :: Symex a PathInfo
getSymexPathInfo = pathInfo `liftM` get

--
-- Queries about types
--

-- | Given a type, get it without its name label.
-- eg
-- %str = type { i32 }
-- getUnnamedTy %str = { i32 }
getUnnamedTy :: Type -> Symex a Type
getUnnamedTy (NamedTypeReference name) = getNamedType name
getUnnamedTy ty                        = return ty

-- | How many elements in the aggregate
numElems :: Type -> Symex a Word32
numElems (VectorType ne _)         = return ne
numElems (ArrayType ne _)          = return $ fromIntegral ne
numElems (StructureType _ elems)   = return $ fromIntegral $ length elems
numElems (NamedTypeReference name) = getNamedType name >>= numElems
numElems _                         = error "Not an aggregate type"

-- | The base type of an aggregate
baseType :: Type -> Symex a (Maybe Type)
baseType ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    VectorType _ ty -> Just ty
    ArrayType _ ty  -> Just ty
    _               -> Nothing

-- | The type of elements in the aggregate
elemTypes :: Type -> Symex a [Type]
elemTypes ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    VectorType numElems ty  -> replicate (fromIntegral numElems) ty
    ArrayType numElems ty   -> replicate (fromIntegral numElems) ty
    StructureType _ elemTys -> elemTys
    _                       -> []

-- | Is the type a pointer type?
isPointerTy :: Type -> Symex a Bool
isPointerTy ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    PointerType{} -> True
    _             -> False

isArrayTy :: Type -> Symex a Bool
isArrayTy ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    ArrayType{} -> True
    _           -> False

-- | Is it an aggregate?
isAggregateTy :: Type -> Symex a Bool
isAggregateTy ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    VectorType{}    -> True
    ArrayType{}     -> True
    StructureType{} -> True
    _               -> False

-- | Is it an aggregate?
isVectorTy :: Type -> Symex a Bool
isVectorTy ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    VectorType{} -> True
    _            -> False

-- | Get the referent type
referentTy :: Type -> Symex a Type
referentTy ty = do
  unnamedTy <- getUnnamedTy ty
  return $ case unnamedTy of
    PointerType refTy _ -> refTy
    _                   -> error "Not a pointer type"

--
-- Monad actions and other utils
--

blankState :: SymexState a
blankState = SymexState {
            attackerState = undefined
            , vars = M.empty
            , tys = M.empty
            , sizes = M.empty
            , offsets = M.empty
            , assigns = M.empty
            , shadowAssigns = M.empty
            , constraints = []
            , memVersions = []
            , memLayout = (defaultDataLayout LittleEndian)
                { typeLayouts = layoutTy
                , pointerLayouts = M.fromList [ (AddrSpace 0, (32, AlignmentInfo 32 32))]
                }
            , pointerSize = 32
            , cellSize = 64
            , shadowMemVersions = []
            , shadowMem = False
            , oobAllowed = False
            , curPointer = 64
            , alreadyAllocd = S.empty
            , canAlias = False
            , strictAllocation = True
            , freshCtr = 0
            , pathInfo = undefined
            , solverTimeout = 300
            , solverResult = SolverFailed "Uninit solver"
            }
  where
    layoutTy = M.fromList [ ((IntegerAlign, 8), (AlignmentInfo 8 8))
                          , ((IntegerAlign, 1), (AlignmentInfo 8 8))
                          , ((IntegerAlign, 16), (AlignmentInfo 16 16))
                          , ((IntegerAlign, 32), (AlignmentInfo 32 32))
                          , ((IntegerAlign, 64), (AlignmentInfo 64 64))
                          , ((IntegerAlign, 128), (AlignmentInfo 128 128))
                          ]

doSymexAction :: SymexState a -> Symex a () -> IO (SymexState a)
doSymexAction ss0 (Symex act) = do
  -- create new boolector state
  bs <- B.newBoolectorState (Just $ solverTimeout ss0)
  -- run the boolector action
  (_, ss1) <- B.evalBoolector bs $ runStateT act ss0
  return ss1

-- | Solve the current symex state
solve :: [Symex a ()] -> Symex a ()
solve refinementActions = do
  sequence_ refinementActions
  s0 <- get
--  forM_ (constraints s0) $ B.assert
  forM_ (M.toList $ assigns s0) $ \(rhs, lhses) -> forM_ lhses $ \lhs -> do
    let var = vars s0 M.! rhs
    B.eq var lhs >>= B.assert
  satResult <- B.sat
  result <- case satResult of
              B.Unsat -> return SolverUnsat
              B.Sat -> do
                assignments <- mapM B.unsignedBvAssignment $ vars s0
                return $ SolverSat assignments
              _ -> return $ SolverFailed "Timed out"
  put $ s0 { solverResult = result }

--
-- The Sat solver results
--

data SolverResult = SolverFailed String
                  | SolverSat (Map VarName Integer)
                  | SolverUnsat
                  deriving (Eq, Show)

-- | Solver result is sat
solverSat :: SolverResult -> Bool
solverSat (SolverSat _) = True
solverSat _             = False

-- | Solver result is unsat
solverUnsat :: SolverResult -> Bool
solverUnsat SolverUnsat = True
solverUnsat _           = False

-- | The solver failed somehow
solverFailed :: SolverResult -> Bool
solverFailed (SolverFailed _) = True
solverFailed _                = False

--
-- Debugging
--

printSymex :: [String] -> Symex a ()
printSymex = liftIO . putStrLn . unlines

printBoolector :: B.Node -> Symex a ()
printBoolector node = do
  boolectorString <- showBoolector node
  printSymex [boolectorString]

showBoolector :: B.Node -> Symex a String
showBoolector node = B.dumpNodeToString B.DumpSMT2 node

dumpConstraints :: Symex a ()
dumpConstraints = do
  constrs <- constraints `liftM` get
  liftIO $ forM_ constrs print
  -- The following is somehow broken:
  -- B.dumpToString B.DumpSMT2 >>= (liftIO . putStrLn)
