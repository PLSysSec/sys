{- |

This module defines data types and functions that encode useful module- and
path-specific information (that our symbolic execution relies on), such as the
types defines in the module(s) and largest GEP index on the path.

-}
module InternalIR.PathInfo ( PathInfo(..)
                           , getPathInfo
                           , mergePathInfos
                           , VarName
                           , makeVarName
                           ) where

import qualified Data.Map              as M
import           Data.Maybe            (catMaybes, fromJust)
import qualified Data.Set              as S
import           Data.Text.Lazy        (unpack)
import           InternalIR.SimplePath
import           LLVM.AST
import qualified LLVM.AST.Constant     as C
import           LLVM.AST.DataLayout
import           LLVM.AST.Type         (Type (..))
import           LLVM.AST.Typed
import           LLVM.Pretty           (ppll)
import           LLVMAST.Interface

-- | Path information, including module information.
data PathInfo = PathInfo {
      pathInfoFile      :: String                     -- ^ Filepath of the module
    , programDataLayout :: Maybe DataLayout           -- ^ Data layout associated with mod
    , programTypes      :: M.Map Name Type            -- ^ Module type aliases
    , programConstants  :: M.Map Name C.Constant      -- ^ Module constants
    , programAliases    :: M.Map Name C.Constant      -- ^ TODO: remove this or use this

    , pathInfoFunc      :: Name                       -- ^ Name of the function
    , variableTypes     :: M.Map VarName Type         -- ^ Function variable types
    , indexBound        :: Integer                    -- ^ Largest GEP index
    , heapAllocSize     :: Integer                    -- ^ Heap allocation
    , parameterTypes    :: M.Map VarName Type         -- ^ Function parameter types
    , castsBetween      :: S.Set (S.Set Type)         -- ^ Casts between different types
    } deriving (Eq, Ord, Show)

-- | Variable names are just Strings (for now).
type VarName = String

-- | Convert between LLVM names and our variable names.
makeVarName :: Name -> VarName
makeVarName = unpack . ppll


-- | Get path info for a function given a module info and simple path in that
-- function.
getPathInfo :: ModuleInfo   -- ^ Module info
            -> Int          -- ^ Path length (from config)
            -> Name         -- ^ Function name
            -> SimplePath   -- ^ Simple path within function
            -> Integer      -- ^ Array bound (from config)
            -> PathInfo
getPathInfo mInfo pathLength funName path arrBound =
  PathInfo { programTypes = modProgramTypes mInfo
           , programDataLayout = moduleDataLayout $ modAST mInfo
           , programConstants = modProgramConstants mInfo
           , programAliases = modProgramAliases mInfo
           , pathInfoFile = show $ moduleSourceFileName $ modAST mInfo
           , variableTypes = getVariableTypes path
           , parameterTypes = getParameterTypes (modAST mInfo) funName
           , castsBetween = getCastsBetween path
           , indexBound = arrBound
           , heapAllocSize = getHeapBound pathLength arrBound path
           , pathInfoFunc = funName
           }

-- | Merge two 'PathInfo's. This is necessary when a path extends beyond a
-- single module.
mergePathInfos :: PathInfo -> PathInfo -> PathInfo
mergePathInfos pi2 pi1 =
  PathInfo { pathInfoFile = pathInfoFile pi1
           , programDataLayout = if programDataLayout pi2 == programDataLayout pi1
                                 then programDataLayout pi1
                                 else error "Can't merge across data layout"
           , programTypes = M.union (programTypes pi1) (programTypes pi2)
           , programConstants = M.union (programConstants pi1) (programConstants pi2)
           , programAliases = M.union (programAliases pi1) (programAliases pi2)
           , pathInfoFunc = pathInfoFunc pi1
           , variableTypes = M.union (variableTypes pi1) (variableTypes pi2)
           , indexBound = max (indexBound pi1) (indexBound pi2)
           , heapAllocSize = max (heapAllocSize pi1) (heapAllocSize pi2)
           , parameterTypes = parameterTypes pi1
           , castsBetween = S.union (castsBetween pi1) (castsBetween pi2)
           }

getVariableTypes :: SimplePath -> M.Map VarName Type
getVariableTypes path =
  M.fromList $ concatMap getInstrInfo $ instrs path
  where
    getInstrInfo instr = case instr of
                           Instr inst          -> concatMap getOperandInfo $ getNonMetaOperands inst
                           PathEq (name := i)  -> [(makeVarName name, typeOf $ head $ getOperands i)]
                           PathNEq (name := i) -> [(makeVarName name, typeOf $ head $ getOperands i)]
                           ParEqs niss         -> concatMap (\(name := inst) ->
                                                    let op = head $ getOperands inst
                                                    in (makeVarName name, typeOf op) -- include BOTH LHS and RHS
                                                        :  getOperandInfo op) $ concat niss
                           TrackOps ops        -> concatMap getOperandInfo ops
                           _                   -> []
    getOperandInfo op = case getOperandName op of
                          Just name -> [(makeVarName name, typeOf op)]
                          Nothing   -> []
    getNonMetaOperands inst = filter (not . isMeta) $ getOperands inst
    isMeta operand = case operand of
                       MetadataOperand _ -> True
                       _                 -> False

-- | Given a Module and a function in that module, get a map from VarName to Type
getParameterTypes :: Module -> Name -> M.Map VarName Type
getParameterTypes modast fName =
  case fromJust $ paramsFromFunctionName modast fName of
    (params, False) -> M.fromList $ map fromParam params
    _               -> error "Variable arguments not yet supported"
  where fromParam (Parameter ty name _) = (makeVarName name, ty)

getCastsBetween :: SimplePath -> S.Set (S.Set Type)
getCastsBetween sp =
    let typeList = map getType $ instrs sp
        typeMap = foldr (M.unionWith S.union) M.empty typeList
        allTypes = updateAllTypes typeMap 5
    in S.fromList $ M.elems allTypes
   where
     getType :: SimpleInstruction
             -> M.Map Type (S.Set Type)
     getType instr = case getInstr instr of
                       Just (BitCast oper1 toType _) ->
                         let fromType = typeOf oper1
                         in if toType == fromType
                            then M.empty
                            else M.fromList [ (toType, S.fromList [toType, fromType])
                                            , (fromType, S.fromList [toType, fromType])
                                            ]
                       _                             -> M.empty
     updateOneType :: M.Map Type (S.Set Type)
                   -> S.Set Type
                   -> S.Set Type
     updateOneType m tys = let updated = S.map (\ty -> M.findWithDefault S.empty ty m) tys
                           in S.unions $ S.toList updated
     updateAllTypes :: M.Map Type (S.Set Type) -> Int -> M.Map Type (S.Set Type)
     updateAllTypes oldMap iters = let newMap = M.map (updateOneType oldMap) oldMap
                             in if oldMap == newMap || iters <= 0
                                then newMap
                                else updateAllTypes newMap (iters - 1)

-- | We want to know the heap allocation bound (ie how far apart we need to space
-- heap allocated pointers). There are two possible situations:
-- (1) All GEPs index with a constant (eg 4). In this case the maximum bound is
-- certainly the sum of all the indecies. (The maximum bound is the true bound in
-- the case where every gep is chained). NOTE: TIMES THE NUMBER OF LOOPS IN THE FUNCTION.
-- The number of loops is certainly <= the block bound, so lets just use that for now
-- (2) One of the GEPs indecies with a variable (eg x). In this case, the maximum
-- bound is the number of GEP instructions in the program * the array bound. Again, the
-- maximum bound is the true bound in the case where every GEP is chained.
getHeapBound :: Int -> Integer -> SimplePath -> Integer
getHeapBound blockbound' defaultInt path =
  let allIndecies = concatMap lookForGepIndex $ instrs path
      staticIndecies = catMaybes allIndecies
      blockbound = fromIntegral blockbound'
  in if length allIndecies == length staticIndecies
     then let maxIndex = sum staticIndecies
          in if maxIndex <= 0 then blockbound else maxIndex * blockbound
     else defaultInt * (fromIntegral $ length allIndecies) * blockbound
  where
    lookForGepIndex instr = case getInstr instr of
                              Just (GetElementPtr _ _ (ind:_) _) ->
                                  case ind of
                                    (ConstantOperand (C.Int _ val)) -> [Just val]
                                    _                               -> [Nothing]
                              _ -> []
