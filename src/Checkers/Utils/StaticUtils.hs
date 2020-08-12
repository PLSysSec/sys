module Checkers.Utils.StaticUtils where
import           Control.Monad       (forM, when)
import           Data.List           (isInfixOf)
import qualified Data.Map            as M
import           Data.Maybe          (fromJust, isJust)
import           LLVM.AST
import           LLVM.AST.Typed
import           LLVMAST.Interface
import           Prelude             hiding (pred)
import           Static.CheckerState

modelAssigns :: Named Instruction -> Checker a b ()
modelAssigns ninstr = do
  case ninstr of
    name := Alloca{}       -> allocVar name
    name := BitCast op _ _ -> addAlias name op
    _                      -> return ()

modelMemory :: Named Instruction -> Checker a b ()
modelMemory = undefined

filterAsm :: Named Instruction -> Checker a b ()
filterAsm ninstr = do
  case ninstr of
        _ | (Call _ _ _ co _ _ _) <- getInstruction ninstr -> when (isAsm co) die
        _ -> return ()

filterFloats :: Named Instruction -> Checker a b ()
filterFloats ninstr = do
  let operands = getOperands ninstr
  when (any (isFloat . typeOf) operands) die

filterAsserts :: Named Instruction -> Checker a b ()
filterAsserts ninstr = when ("assert_fail" `isInfixOf` show ninstr) $ die

-- | Get paths from a given program point
getPathsFrom :: Bool -> M.Map Name [Name] -> M.Map Name [Name] -> [Name] -> IO [[Name]]
getPathsFrom _haveLooped phis preds path
  | not $ M.member block preds = return [path]
  | length path > 15 = return [[]]
  | otherwise = do
      let curPreds = if M.member block phis
                     then phis M.! block
                     else preds M.! block
          predsNoLoops = filter (/= block) curPreds
          newPaths = map (:path) predsNoLoops
      result <- mapM (getPathsFrom True phis preds) newPaths
      return $ filter (not . null) $ concat result
  where
    block = head path

getPredMap :: Checker a b (M.Map Name [Name])
getPredMap = do
  blocks <- getBlockList
  preds <- forM blocks $ \block -> let nextBlocks = getNextBlocksOf block
                                       predName = getBlockName block
                                   in return $ map (\nb -> (nb, predName)) nextBlocks
  return $ foldr (\(bname, pred) newMap ->
                      M.insertWith (++) bname [pred] newMap
                 ) M.empty $ concat preds

-- | Given a referent type being indexed into by a GEP instuction, and a list of index
-- operands, return a list of the types accessed. For example:
-- GEP { i32, [2 x i1] } *x, 0, 1, 0
-- 'type' is { i32, [2 x i1]}, 'ops' are [1, 0]
-- The result is [{i32, [2 x i1]}, [2 x i1 ], i1]
getInnerTypes :: Type
              -> [Operand]
              -> Checker a b [Type]
getInnerTypes ty ops = getTypes ty ops [ty]

getType :: Type
        -> Checker a b Type
getType (NamedTypeReference name) = do
  unnamedTy <- getNamedType name
  case unnamedTy of
    Just ty' -> return ty'
    _        -> error "Didn't find name for type"
getType t = return t

getTypes :: Type
         -> [Operand]
         -> [Type]
         -> Checker a b [Type]
-- Filter out nested arrays, since you get to have all sorts of bounds rules there
getTypes (ArrayType _ ArrayType{}) _ _ = return []
getTypes (ArrayType _ elemType) (_:inds) tys = do
  ty <- getType elemType
  getTypes elemType inds $ tys ++ [ty]
getTypes (VectorType _ elemType) (_:inds) tys = do
  ty <- getType elemType
  getTypes elemType inds $ tys ++ [ty]
getTypes s@(StructureType _ elemTypes) (ind:inds) tys =
  if isConstant ind
  then let indexNum = fromJust $ getConstant ind
       in if indexNum >= length elemTypes
          then error "Out of bounds struct index"
          else let nextType = elemTypes !! indexNum
               in if s `elem` tys
                  then return []
                  else do
                    ty <- getType nextType
                    getTypes nextType inds $ tys ++ [ty]
  else error "Cannot index into struct with symbolic type"
getTypes n@NamedTypeReference{} inds tys = do
  ty <- getType n
  getTypes ty inds tys
getTypes _ [] tys = return tys
getTypes ty inds _ = error $ unwords [ "Malformed type"
                                     , show ty
                                     , "with indecies"
                                     , show inds
                                     ]

getNumElems :: Type -> Checker a b (Maybe Int)
-- Don't bother filtering nested arrays, since we did that in the previous step
-- getNumElems (ArrayType _ ArrayType{}) = return Nothing
-- This could be a dynamically sized array. O no!
getNumElems (ArrayType 1 _) = return Nothing
getNumElems (ArrayType 0 _) = return Nothing
getNumElems (ArrayType numElems _) = return $ Just $ fromIntegral numElems
getNumElems (VectorType numElems _) = return $ Just $ fromIntegral numElems
getNumElems (NamedTypeReference name) = do
  ty <- getNamedType name
  if isJust ty then getNumElems $ fromJust ty else return Nothing
getNumElems _ = return Nothing
