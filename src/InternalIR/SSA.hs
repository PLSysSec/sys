{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{- |

This module implement a single-static-assignment form for 'SimplePath's and
LLVM's basic blocks.

-}
module InternalIR.SSA (toSSA) where

import qualified Control.Monad.Fail         as Fail
import           Control.Monad.State.Strict
import qualified Data.List                  as List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import qualified Data.Set                   as Set
import           InternalIR.SimplePath
import           LLVM.AST
import           LLVM.AST.Typed
import           LLVMAST.ASTInterface
import           LLVMAST.RenameVariables    hiding (rewriteNamedInstruction)
import qualified LLVMAST.RenameVariables    as R

class ToSSA a where
  -- | The simple path may assign a single variable multiple times. Here we
  -- rename variables and turn the path into SSA form.
  toSSA :: a -> a

instance ToSSA SimplePath where
  toSSA sp =
    let si = catMaybes . evalSSAMonad $ mapM rewriteSimpleInstruction $ instrs sp
    in sp { instrs = si }

instance ToSSA [BasicBlock] where
  toSSA bbs0 =
    let rewriteBB (BasicBlock name instrs0 term0) = do
          instrs1 <- mapM rewriteNamedInstruction instrs0
          term1 <- rewriteNamedTerminator term0
          return $ BasicBlock name instrs1 term1
    in evalSSAMonad $ mapM rewriteBB bbs0

-- | The SSAMonad keeps track of a global map of variables to count and list of
-- name maps. We use the global variable to ensure unique names across calls to
-- the same function. We use the list of name maps to ensure that exiting a
-- call won't lead to using the newer names (if they're from the same
-- function).
type SSAMonad = StateT (Map Name Int, [NameMap]) RenameMonad

instance {-# OVERLAPPING #-} Fail.MonadFail SSAMonad where
  fail = Fail.fail

evalSSAMonad :: SSAMonad a -> a
evalSSAMonad act = evalState (evalStateT act (Map.empty, [])) Map.empty

-- | Update the global variable counter
updateVarCounter :: Name -> SSAMonad Int
updateVarCounter n = do
  (cntMap0, nMap) <- get
  let cnt     = maybe 1 (+1) $ Map.lookup n cntMap0
      cntMap1 = Map.insert n cnt cntMap0
  put (cntMap1, nMap)
  return cnt

rewriteSimpleInstruction :: SimpleInstruction -> SSAMonad (Maybe SimpleInstruction)
rewriteSimpleInstruction (Instr ni)  = (Just . Instr) <$> rewriteNamedInstruction ni
rewriteSimpleInstruction (PathEq ni) = (Just . PathEq) <$> (lift . R.rewriteNamedInstruction) ni
rewriteSimpleInstruction (PathNEq ni) = (Just . PathNEq) <$> (lift . R.rewriteNamedInstruction) ni
rewriteSimpleInstruction si@(ParEqs _) = rewriteParEqs si
rewriteSimpleInstruction (TrackOps ops0) = (Just . TrackOps) <$> mapM (lift . rewriteOperand) ops0
rewriteSimpleInstruction node@(EnterCall _) = do
  -- get the current map
  nMap <- lift get
  -- add it to "stack frame"
  modify $ \(cnt, nMaps) -> (cnt, nMap:nMaps)
  return $ Just node
rewriteSimpleInstruction (ExitCall mRetName0) = do
  -- get the current map
  nMap1 <- lift get
  -- get most recent map
  (cnt, nMap0:rest) <- get
  put (cnt, rest)
  -- restore the map, but take any new names too; this is useful for argument
  -- stitching (formal = actual) which we do by prefixing a few instructions to
  -- the start of a the first BB of the entered function.
  let nMap2 = nMap0 `Map.union` nMap1
      -- multiple calls to same function could result in overpopping
      (mRetName1, nMap3) = case mRetName0 of
                             Just retName | (Just retVal) <- Map.lookup retName nMap1
                                         -> (Just $ rewriteNameToName retVal, Map.insert retName retVal nMap2)
                             _ -> (mRetName0, nMap2)
  lift $ put nMap3
  return $ Just (ExitCall mRetName1)

-- | Rewrite the variables in a ParEqs. Each list of instructions in a par-eqs
-- corresponds to a phi-path, i.e., the variables used and defined in a phi
-- node if we had take one branch. Since we may use a variable in one path, but
-- not the other, we need to make sure that the counting in each path is the
-- same. Consider:
--
-- >  x = phi  [(y, one), (z, two)]
-- >  y = phi  [(z, one), (x, two)]
-- >  u = bitcast y
--
-- Here, y is used on path one, but not two. This means that if we had taken path one, we'd have:
--
-- > x = y AND y_1 = z AND u = y_1
--
-- And if we had taken path two, we'd have:
--
-- > x = z AND y = x AND u = y
--
-- So, really, u = y OR u = y_1 depending on the path. Since par-eqs are the
-- only way to express multiple paths we need to make sure the paths merge.
-- This renaming function does precisely this. In our example, it does so by
-- "extending" path two with the equation y_1 = y, leading to the final:
--
-- > (x = y AND y_1 = z AND u = y_1) OR (x = z AND y = x AND u = y AND y_1 = y)
--   ^^^^^^^^^^path one^^^^^^^^^^^^^    ^^^^^^^^^^^^path two^^^^^^^^^^^^^^^^^^^
--
-- NOTE: we assume that ParEqs only consist of bitcasts created from phi nodes
-- with phisToPar. Moreover, we assume they are only used for paths that start
-- form a block with phi nodes.
rewriteParEqs :: SimpleInstruction -> SSAMonad (Maybe SimpleInstruction)
rewriteParEqs (ParEqs niss0) = do
  -- Get initial state
  s0 <- getState
  -- Get renamed instructions and underlying monad state for each path
  stateAndInstrs <- forM niss0 $ \nis0 -> do
    -- Rename each instruction
    nis1 <- mapM rewriteNamedInstruction nis0
    -- Get the final name state (**)
    s1 <- getState
    -- Reset the state of this path to original
    putState s0
    -- Return the variables renamed and renamed instructions
    return (s1, stateToVars s1, nis1)
  -- Get all the variables defined across all paths
  let allVars = Set.unions $ map (\(_, vs, _) -> vs) stateAndInstrs
  -- For each path, define new variables that appear in other paths
  niss1 <- forM stateAndInstrs $ \(s1, vars, nis0) -> do
    let newVars = Set.toList $ allVars Set.\\ vars
    -- Restore state of path (**)
    putState s1
    -- Rename each variable
    newNis <- forM newVars $ \var ->
      let ty = getTyOfVarFromPrevUse var nis0
      in rewriteNamedInstruction $ setEq' var ty (LocalReference ty var) []
    -- Reset the state of path to original
    putState s0
    -- Append new instructions to path
    return $ nis0 ++ newNis
  -- Merge all the states and set the underlying state
  let mergeState = foldr mergeStates s0 $ map (\(s,_,_) -> s) stateAndInstrs
  -- Merging of the states only makes sense when we're not in a call.
  unless (let ((_,nMaps),_) = s0 in null nMaps) $ error "BUG: ParEqs used in unexpected way"
  -- Set the underlying state to the merged state
  putState mergeState
  -- Return the instructions
  return $ Just $ ParEqs niss1
    where getState = do s <- get
                        nMap <- lift get
                        return (s, nMap)
          putState (s, nMap) = put s >> lift (put nMap)
          -- Merge two states.
          -- NOTE: we don't merge the name maps since we assume par-eqns are
          -- only used when we start from a block with phi nodes. Indeed the
          -- merging of the counter maps may also be totally wrong if this
          -- assumption is violated.
          mergeStates ((cntMap0, _), nMap0) ((cntMap1, _), nMap1) =
            ((Map.union cntMap0 cntMap1, []), Map.union nMap0 nMap1)
          -- Given a ssa state, get a set of the variables defined
          stateToVars = Set.fromList . Map.keys . fst . fst
          -- Get the type of a variable from it's previous usage.
          -- NOTE: we assume that par-eqns are from phi nodes and thus always
          -- bitcasts.
          getTyOfVarFromPrevUse :: Name -> [Named Instruction] -> Type
          getTyOfVarFromPrevUse vn nis =
            case List.find (\ni -> Just vn == getResultName ni) nis of
              Just ni -> typeOf $ head $ getOperands ni
              _ -> error $ "BUG: getTyOfVarFromPrevUse: expected variable " ++ show vn ++ "to have been used"

rewriteParEqs si = error $ "BUG: called rewriteParEqs on something other than ParEqs: " ++ show si

rewriteNamedInstruction :: Named Instruction -> SSAMonad (Named Instruction)
rewriteNamedInstruction (n0 := i0) = do
  i1 <- lift $ rewriteInstruction i0
  addToNameMap $ namesOf $ getOperands i0
  n1 <- rewriteNameAndUpdate n0
  return $ n1 := i1
rewriteNamedInstruction (Do i0) = do
  i1 <- lift $ rewriteInstruction i0
  addToNameMap $ namesOf $ getOperands i0
  return $ Do i1

rewriteNamedTerminator :: Named Terminator -> SSAMonad (Named Terminator)
rewriteNamedTerminator (n0 := t0) = do
  t1 <- lift $ rewriteTerminator t0
  addToNameMap $ namesOf $ getOperands t0
  n1 <- rewriteNameAndUpdate n0
  return $ n1 := t1
rewriteNamedTerminator (Do t0) = do
  t1 <- lift $ rewriteTerminator t0
  addToNameMap $ namesOf $ getOperands t0
  return $ Do t1

-- | This renames an existing name by incrementing counter or returns original
-- name if this is the first assignment.
rewriteNameAndUpdate :: Name -> SSAMonad Name
rewriteNameAndUpdate n0 = do
  mRn0 <- lift $ getRewriteName n0
  rn1  <- case mRn0 of
              Nothing -> return $ RewriteName Nothing n0 Nothing
              Just rn0 -> do cnt <- updateVarCounter n0
                             return $ rn0 { rnVer = Just cnt }
  lift $ putRewriteName n0 rn1
  return $ rewriteNameToName rn1

-- | Add names to map if they're not already there
addToNameMap :: [Name] -> SSAMonad ()
addToNameMap ns = do
  lift $ forM_ ns $ \n0 -> do
    mRn0 <- getRewriteName n0
    case mRn0 of
      Nothing -> putRewriteName n0 $ RewriteName Nothing n0 Nothing
      Just _  -> return ()
