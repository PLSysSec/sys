{-|

This static checker looks for memory operations that are 'concretely' out-of-bounds---like
``GEP [5 x i32] x, 0, 6'', which is one off the end of x.

There are three cases it detects:
1. Out-of-bounds with a constant, like in the example above
2. Out-of-bounds with a negative, like GEP x -1
3. Out-of-bounds with an undef constant, like GEP x undef

The checker works in two phases.
First, it sets up the state it needs to identify bugs in the setupState function.
Then, it looks for bugs using the concreteOOBCheck function.

-}

module Checkers.ConcreteOOBStatic where
import           Checkers.Utils.StaticUtils
import           Control.Monad              (forM, forM_, unless, when)
import           Control.Monad.State.Strict (liftIO, liftM)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import qualified Data.Set                   as S
import           LLVM.AST                   hiding (index)
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getFunName)
import           Static.CheckerState        hiding (addAlias)

--
-- Checker state
--

data COOBInfo = COOBInfo { constantVars :: M.Map Name (S.Set Integer)
                         -- ^ Association between variables names and the set of constants
                         -- that they may take on
                         , undefVars    :: S.Set Name
                         , couldBeBugs  :: [COOBBug]
                         -- ^ Set of variables that may be undef in the given function
                         , couldBeOk    :: S.Set Name
                         -- ^ Set of variables that could be ok. For example:
                         -- Geps:
                         --   x = GEP y 1 2 3
                         --   In the above, x is defined by a GEP instruction
                         -- Phis:
                         --   since they may have come from GEPs and we have to be
                         --   conservative.
                         -- Arguments:
                         --   we don't know anything about the expected structure of args
                         -- Structs and classes
                         --   these can be structured so OOB is ok
                         , phiChoices   :: M.Map Name [Name]
                         -- ^ Map of blocks on their corresponding phi nodes
                         , predBlocks   :: M.Map Name [Name]
                         -- ^ Map between blocks in the function and their predecessors
                         }
               deriving (Eq, Ord, Show)

blankInfo :: COOBInfo
blankInfo = COOBInfo M.empty S.empty [] S.empty M.empty M.empty

-- | Add a constant to constantVars, the map of constants in the checker state
addConstant :: Name -> Operand -> Checker COOBInfo b ()
addConstant name op = do
  s0 <- getState
  let newConstant      = S.fromList [fromJust $ getConst op]
      constants        = constantVars s0
      updatedConstants = M.insertWith S.union name newConstant constants
  putState $ s0 { constantVars = updatedConstants }

-- | Given an operand, return Just its constant value, if it can be a constant, or
-- Nothing, if it is neither a constant nor a variable in the constantVars map
getConstants :: Operand -> Checker COOBInfo b (S.Set Integer)
getConstants op =
  case op of
    _ | isConstant op -> return $ S.fromList [fromJust $ getConst op]
    _ | isLocalReference op -> do
      s0 <- getState
      let name = nameOf' op
      return $ M.findWithDefault S.empty name $ constantVars s0
    _ -> return S.empty

-- | Add a variable name to the set of vars that can be undef, undefVars
addUndef :: Name -> Checker COOBInfo b ()
addUndef name = do
  s0 <- getState
  putState $ s0 { undefVars = S.insert name $ undefVars s0 }

-- | Check if a variable is undef
isUndefVar :: Operand -> Checker COOBInfo b Bool
isUndefVar op =
  case op of
    _ | isLocalReference op -> do
      s0 <- getState
      return $ S.member (nameOf' op) $ undefVars s0
    _ | isUndef op -> return True
    _ -> return False

addMaybeBug :: COOBBug -> Checker COOBInfo b ()
addMaybeBug bug = do
  s0 <- getState
  putState $ s0 { couldBeBugs = bug:couldBeBugs s0 }

getMaybeBugs :: Checker COOBInfo b [COOBBug]
getMaybeBugs = couldBeBugs `liftM` getState

-- | Add a variable to the set of vars that can have OOB indecies
addOk :: Name -> Checker COOBInfo b ()
addOk name = do
  s0 <- getState
  putState $ s0 { couldBeOk = S.insert name $ couldBeOk s0 }

-- | Is this operand in the OK set?
wasOk :: Operand -> Checker COOBInfo b Bool
wasOk op =
  case op of
    _ | isLocalReference op -> do
      s0 <- getState
      return $ S.member (nameOf' op) $ couldBeOk s0
    _ -> return False

-- | Add an association between this block and the previous block
addPhiChoice :: Name -> Name -> Checker COOBInfo b ()
addPhiChoice curBlockName prevBlockName = do
  state <- getState
  let choices = phiChoices state
      updated = M.insertWith (++) curBlockName [prevBlockName] choices
  putState $ state { phiChoices = updated }

-- | Look at each block in the program (in no order) to setup the state.
-- This means setting up:
--
-- 1. constantVars, the map of all variables that can be assigned to
-- constants as the result of phi nodes. For example, ``x = phi [..,4], [...6]''
-- would yield a map entry of (x, {4, 6}).
--
-- 2. undefVars, the set of all variables that can be assigned undef at phis.
-- For example, ``x = phi [..undef]'' would cause setupState to add x to undefVars.
--
-- 3. definedByGep, the set of all variables that are defined by GEP operations.
-- This is important for false positive suppression, since jumping out-of-bounds
-- off of variables that are *already* defined by GEPs can be ok. For example:
-- x = GEP z 1
-- y = GEP x -1
-- This just means that y and z alias one another, which is fine.
--
-- 4. phiChoices, a map of all the phis and the blocks they are associated with.
-- We just use this to determine a path from a potential bug back to program entry
setupState :: Name -> Named Instruction -> Checker COOBInfo b ()
setupState block ninstr = do
  case ninstr of

    -- Setup constantVars, undefVars, and phiChoices
    name := Phi _ ops _ -> forM_ ops $ \(val, label) -> do
      addOk name
      when (isInt $ typeOf val) $ do
        when (isUndef val) $ addUndef name
        when (isConstant val) $ addConstant name val
        addPhiChoice block label

    -- Setup geps
    name := GetElementPtr{} -> addOk name

    -- Mark certain things as ok
    name := _ -> do
      let ops = getOperands ninstr
      forM_ ops $ \ op -> do
        let ty = typeOf op
        when ("class" `isInfixOf` show ty || "struct" `isInfixOf` show ty) $ do
          when (isLocalReference op) $ addOk $ nameOf' op
          addOk name
        ok <- wasOk op
        when ok $ addOk name

    _ -> return ()



--
-- The check itself
--

data COOBBug = COOBBug { negVar      :: Name
                       -- ^ Name of the result of the OOB operation
                       -- For example, if the following is OOB, x is the name:
                       -- x = GEP y OOB indecies
                       , negIndex    :: (Either Name Integer)
                       -- ^ Either the name of the variable causing the OOB or
                       -- the value of the constant causing the OOB
                       , negAmount   :: Integer
                       -- ^ The size of the OOB index
                       , negPath     :: Path
                       , negFunction :: Name
                       , negFile     :: String
                       }
               deriving (Eq, Ord, Show)

-- | Check every basic block of the function
checkAll :: Checker COOBInfo COOBBug ()
checkAll = do
  state <- getState
  predMap <- getPredMap
  putState $ state { predBlocks = predMap }
  blocks <- getBlockList
  params <- getParams
  -- You can tweak this for the types you want depending on your use case.
  forM_ (M.toList params) $ \(name, _ty) -> addOk name
  forM_ blocks $ \block -> forM_ (getBlockContents block) (setupState $ getBlockName block)
  forM_ blocks $ concreteOOBCheck
  bugs <- getMaybeBugs
  forM_ bugs $ \bug -> do
    uses <- getUses blocks bug >>= return . catMaybes . concat
    unless (null uses || all safeUses uses) $ addBug bug
  die
  where getUses blocks bug = do
          forM blocks $ \block ->
              forM (getBlockContents block) $ \ninstr -> do
                let ops = catMaybes $ map nameOf $ getOperands ninstr
                if (negVar bug) `elem` ops
                then return $ Just ninstr
                else return Nothing
        safeUses ninstr = case ninstr of
                             _ := Load{}  -> False
                             Do (Store{}) -> False
                             _            -> True


-- | Check for out-of-bounds bugs caused concretely OOB values
concreteOOBCheck :: BasicBlock -> Checker COOBInfo COOBBug ()
concreteOOBCheck bb = forM_ (getBlockContents bb) $ \ninstr -> do

  case ninstr of

    name := GetElementPtr _ addr (ind:inds) _ -> do

      let refTy = getReferentType $ typeOf addr
      ok <- wasOk addr
      types' <- getInnerTypes refTy inds
      let types = if null types' then [] else init types'

      -- Check that the "array" index isn't negative or undef
      unless ok $ checkNegativeInd name ind
      checkUndefInd name ind

      forM_ (zip inds types) $ \(innerInd, ty) -> do
        mNumElems <- getNumElems ty
        constants <- getConstants innerInd

        -- Check for undef indecies
        checkUndefInd name innerInd

        -- Check for indexing with an OOB constant
        when (isJust mNumElems && not (S.null constants)) $ do
          let numElems = fromIntegral $ fromJust mNumElems

          forM_ constants $ \constant ->
            -- Check for positive OOB
            when (constant >= numElems && not ok) $ addConstBug bb name innerInd constant

    _ -> return ()

  where
    isNegativeNum ty c = case ty of
                           IntegerType 8  -> c > 127
                           IntegerType 16 -> c > 32767
                           IntegerType 32 -> c > 2147483647
                           IntegerType 64 -> c > 18446181123756130303
                           _              -> False

    checkNegativeInd name index = do
      let indexType = typeOf index
      constants <- getConstants index
      -- Concretely negative index
      when (isNegative index) $ do
        addConstBug bb name index (fromJust $ getConst index)
        -- Negative index using a variables
        forM_ constants $ \c -> when (isNegativeNum indexType c) $ addConstBug bb name index c

    checkUndefInd name index = do
      isUndefIndex <- isUndefVar index
      -- Either concretely OOB or OOB via a phi assignment
      when (isUndef index || isUndefIndex) $ addConstBug bb name index 0

-- | Add a bug signifying a concretely OOB access
addConstBug :: BasicBlock -> Name -> Operand -> Integer -> Checker COOBInfo COOBBug ()
addConstBug bb resultVar op size = do
  let index = case op of
                _ | isUndef op    -> Right 0
                _ | isConstant op -> Right $ fromJust $ getConst op
                _ -> Left $ fromJust $ nameOf op
  paths <- getPaths bb
  func <- getFunName
  file <- getFilepath
  forM_ paths $ \p -> addMaybeBug $ COOBBug resultVar index size p func file
--    addBug $ COOBBug resultVar index size p func file

-- | Get a path backwards from a given basic block to the entry point
getPaths :: BasicBlock -> Checker COOBInfo COOBBug [Path]
getPaths bb = do
  preds <- predBlocks `liftM` getState
  phis <- phiChoices `liftM` getState
  paths <- liftIO $ getPathsFrom False phis preds [getBlockName bb]
  return paths
