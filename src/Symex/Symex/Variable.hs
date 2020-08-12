{-|

Implements operations on variables: the first part of the file is creation,
second is assignment, and third is determining the sizes (widths) of different
vars.  Deteremining size is the most complicated portion of the file by far.

-}
module Symex.Symex.Variable ( -- * Making variables
                              getVariable
                            , getVariableFromProgram
                            , freshVar
                              -- * Assigning variables and asserting things
                            , assignVar
                            , assign
                            , assert
                              -- * Types and their sizes
                            , typeToSort
                            , sizeOf
                            , maxSizeOf
                            , sizeOfVectorElem
                              -- * Calculating offsets into a structure based on types
                            , offsetsOf
                            , offsetOfIndex
                            ) where
import           Control.Monad                (when)
import           Control.Monad.State.Strict
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust, isJust)
import qualified Data.Set                     as S
import           Data.Word                    (Word32, Word64)
import           InternalIR.PathInfo
import           LLVM.AST                     hiding (index)
import           LLVM.AST.AddrSpace
import           LLVM.Context
import           LLVM.Internal.Coding
import           LLVM.Internal.DataLayout
import           LLVM.Internal.EncodeAST
import           LLVM.Internal.FFI.DataLayout hiding (DataLayout)
import           LLVMAST.Interface            (isVector)
import qualified Symex.Symex.Boolector        as B
import           Symex.Symex.SymexState       hiding (numElems)
import           Symex.Symex.Utils

--
-- Creation
--

-- | If a variable of name llvmName and type ty exists, return it.
-- Otherwise, make it, and return the newly made variable.
getVariable :: VarName -> Type -> Symex a B.Node
getVariable varName ty = do
  s0 <- get
  let allVars = vars s0
  case M.lookup varName allVars of
    Just existingVar -> return existingVar
    Nothing -> do
      let allTys = tys s0
      sort <- typeToSort ty
      newVar <- B.var sort varName
      put $ s0 { vars = M.insert varName newVar allVars
               , tys = M.insert varName ty allTys
               }
      return newVar

-- | Get a variable using *just* its varname (ie not its type).
-- For this to be possible, we have to have a record of the variable's
-- type---so the variable must be used in the path we are symbolically executing.
-- Therefore, we can get the type from the path info, and return the variable.
-- If the type does not exist in the pathInfo, throw an error.
getVariableFromProgram :: VarName -> Symex a B.Node
getVariableFromProgram varName = do
  s0 <- get
  let allVars = vars s0
  case M.lookup varName allVars of
    Just var -> return var
    Nothing  -> do
      let allProgramVars = variableTypes $ pathInfo s0
      case M.lookup varName allProgramVars of
        Just ty -> getVariable varName ty
        Nothing -> error $ unlines $ [ varName ++ " does not exist in program"
                                     , "Program variables:"
                                     , show allProgramVars
                                     ]

-- | Create a new, unique variable of a given type.
freshVar :: Type -> Symex a B.Node
freshVar ty = do
  newFreshName <- freshName
  getVariable newFreshName ty

-- | Translate an LLVM hs type to a Boolector sort
typeToSort :: Type -> Symex a B.Sort
typeToSort ty = sizeOf ty >>= B.bitvecSort . fromIntegral

--
-- Assigning and asserting
--

assignVar :: Name
          -> B.Node
          -> Symex a ()
assignVar name rhs = do
  let varName = makeVarName name
  lhs <- getVariableFromProgram varName
  assign lhs rhs
  s0 <- get
  put $ s0 { assigns = M.insertWith S.union varName (S.fromList [rhs]) $ assigns s0 }

-- | Assign an expression to a variable
assign :: B.Node -- ^ LHS
       -> B.Node-- ^ RHS
       -> Symex a () -- ^ Assignment assigns the RHS to the LHS,
                     -- and ALSO adds the constraint to the list.
                     -- It is the complete action of assignment.
assign lhs rhs = B.eq lhs rhs >>= assert

-- | Assert an expression
assert :: B.Node -- ^ Statement to assert
       -> Symex a () -- ^ It adds the constraint to the list,
                     -- so it is the complete action of assertion
assert c = addConstraint c >> B.assert c

--
-- Sizing.
--

-- | Size of a type
sizeOf :: Type -> Symex a Word32
sizeOf ty = unnameType ty >>= sizeOf'

-- | Determine the maximum allocation size of a type.
-- This is usually the size of the type, unless the type is bitcast to something
-- larger later on in the code snippet.
maxSizeOf :: Type -> Symex a Word32
maxSizeOf ty = do
  casts <- castsBetween `liftM` pathInfo `liftM` get
  let dummyType = PointerType ty $ AddrSpace 0
  dummySize <- sizeOf ty
  castToSizes <- forM (S.toList casts) $ \setOfCasts -> do
    if S.member dummyType setOfCasts
    then do
      otherSizes <- forM (S.toList setOfCasts) $ \castToTy -> do
        refTy <- referentTy castToTy
        sizeOf refTy
      return $ maximum otherSizes
    else return dummySize
  return $ case castToSizes of
    [] -> dummySize
    _  -> maximum castToSizes

-- | Get (and possibly record) the size of a type.
-- For aggregates, this function:
-- (1) calculates the size of the aggregate type and
-- (2) updates the sizes and offsets map to record both the size of
--     the element and the indecies of its offsets
sizeOf' :: Type -> Symex a Word32
sizeOf' ty = do
  s0 <- get
  let allSizes = sizes s0
      allOffsets = offsets s0
  case M.lookup ty allSizes of
    Just size -> return size
    Nothing -> do
      isAggregate <- isAggregateTy ty
      isPointer <- isPointerTy ty
      newOffsets <- if isAggregate then ffiGetOffsets ty else return []
      size <- if isPointer then return $ pointerSize s0 else ffiGetSize ty
      put $ s0 { sizes = M.insert ty size allSizes
               , offsets = M.insert ty newOffsets allOffsets
               }
      return size

-- | Given a type, get the fully unnamed version of that type
-- ie the version with no NamedTypeReference types nested within it
unnameType :: Type -> Symex a Type
unnameType ty = do
  unnamedTy <- getUnnamedTy ty
  case unnamedTy of
    VectorType numElems elemTy -> do
      unless (numElems > 0) $ error "Do not support zero-length vectors"
      unnameType elemTy >>= return . VectorType numElems
    ArrayType numElems elemTy -> do
      unless (numElems > 0) $ error "Do not support zero-length arrays"
      unnameType elemTy >>= return . ArrayType numElems
    StructureType packed elemTys -> do
      types <- mapM unnameType elemTys
      return $ StructureType packed types
    -- All pointers will have the same size regardless of their pointee
    -- Telling this lie allows us to support types like linked lists
    PointerType _ addrSpace -> return $ PointerType (IntegerType 32) addrSpace
    FunctionType rty atys isVa -> do
      unnamedR <- unnameType rty
      unnamedAs <- mapM unnameType atys
      return $ FunctionType unnamedR unnamedAs isVa
    _ -> return unnamedTy

-- | Ask LLVM the size of a type. This function hits the LLVM bindings
ffiGetSize :: Type -> Symex a Word32
ffiGetSize FunctionType{} = return 32
ffiGetSize ty = do
  dl <- memLayout `liftM` get
  size <- liftIO $ withContext $ \ctx -> runEncodeAST ctx $ do
    ety <- encodeM ty
    liftIO $ withFFIDataLayout dl (\layout -> getTypeAllocSize layout ety)
  return $ 8 * fromIntegral size

-- | Vector elements are not just the size of their type.
-- The size changes depending on the number of elements in the vector, etc.
-- That's why we need a seperate function that gets the sizes of vector element types
sizeOfVectorElem :: Type -> Symex a Word64
sizeOfVectorElem ty@(VectorType numElems _) = do
  totalSize <- sizeOf ty
  return $ fromIntegral $ totalSize `div` numElems
sizeOfVectorElem _ = error "Not a vector type"

-- | Calculate the indecies of offsets in a type (for vectors and arrays)
-- For structs, ask LLVM the offsets.
ffiGetOffsets :: Type -> Symex a [Word64]
ffiGetOffsets ty =
  case ty of
    -- Vector sizing is not well documented
    -- The element size will vary depending on the layout the compiler chooses
    -- That's why our code for vectors is different from that of arrays
    VectorType numElems' _   -> do
      totalSize <- ffiGetSize ty
      -- Note that we CANNOT ask the FFI here, because the compiler chooses the
      -- size+padding itself
      let elemSize = fromIntegral $ totalSize `div` numElems'
          numElems = fromIntegral numElems'
      return $ map ( * elemSize) [0..numElems - 1]
    ArrayType numElems elemTy    -> do
      elemSize <- ffiGetSize elemTy
      return $ map ( * fromIntegral elemSize) [0..numElems - 1]
    StructureType _ elemTys -> do
      let inds = [0..length elemTys - 1]
      dl <- memLayout `liftM` get
      structOffsets <- liftIO $ withContext $ \ctx -> runEncodeAST ctx $ do
        ety <- encodeM ty
        liftIO $ forM inds $ \ind ->
          withFFIDataLayout dl (\layout -> getOffsetOfElement layout ety $ fromIntegral ind)
      return $ map (*8) structOffsets
    _ -> error "No aggregate type"

-- | Gets (and possibly) records the offsets of the indecies in an element
-- If no index offsets are recorded, it calls "sizeOf," which does all the
-- size and offset record keeping. Then it recurses into "offsetsOf"
offsetsOf :: Type -> Symex a [Word64]
offsetsOf IntegerType{} = error "No offsets in an int"
offsetsOf PointerType{} = error "No offsets in a pointer"
offsetsOf ty = do
  s0 <- get
  unnamed <- unnameType ty
  let allOffsets = offsets s0
  case M.lookup unnamed allOffsets of
    Just tyOffsets -> return tyOffsets
    Nothing        -> sizeOf ty >> offsetsOf ty

-- | Given an index into a structure (e.g, { i32, i32 } and 1), produce
-- the bit offset corresponding to that index (e.g., 32).
offsetOfIndex :: B.Node -- ^ The index
                -> Type -- ^ The type into which we are indexing
                -> Symex a B.Node -- ^ The offset
offsetOfIndex index aggregateTy = do
  indexVal <- B.unsignedBvConst index
  tyOffsets <- offsetsOf aggregateTy
  case aggregateTy of
    VectorType{} -> vectOrArrIdx aggregateTy tyOffsets indexVal
    ArrayType{}  -> vectOrArrIdx aggregateTy tyOffsets indexVal
    StructureType{}  | isJust indexVal -> do
      let indexConst = fromIntegral $ fromJust indexVal
      when (indexConst >= length tyOffsets) $ error "OOB index in offsetOfIndex"
      let offset = tyOffsets !! indexConst
      pointerWidth <- getPointerSize
      numberWidth pointerWidth $ fromIntegral offset
    NamedTypeReference name -> getNamedType name >>= offsetOfIndex index
    _                          -> error $ unlines ["Malformed index into aggregate"
                                                  , show aggregateTy
                                                  , show index
                                                  ]
  where
    vectOrArrIdx :: Type -> [Word64] -> (Maybe Integer) -> Symex a B.Node
    vectOrArrIdx ty tyOffsets maybeIdx = do
      pointerWidth <- getPointerSize
      innerTys <- elemTypes ty
      size <- if isVector ty
              then sizeOfVectorElem ty >>= return . fromIntegral
              else sizeOf $ head innerTys
      if isJust maybeIdx
      -- The index is a constant: just get the proper offset
      then do
        let idx = fromIntegral $ fromJust maybeIdx
        -- We have to allow the index to be OOB or we are unable
        -- to detect OOB errors that use a constant. Similarly, there are
        -- some legitimate ways to have an oob offset.
        numberWidth pointerWidth $ fromIntegral $ size * idx
      -- Constrain the index to only be a real, possible index
      else do
        -- The result of the constraint math on this item
        finalIndex <- freshVar $ IntegerType pointerWidth
        -- Make on offset out of the index
        castIndex <- B.castToWidth index pointerWidth
        mulIndex <- numberWidth pointerWidth (fromIntegral size) >>= B.mul castIndex
        -- Constrain it to be only a feasible index
        possibleOffsetSyms <- mapM (\off -> numberWidth pointerWidth (fromIntegral off) >>=
                              B.eq finalIndex) tyOffsets
        orNodes possibleOffsetSyms >>= assert
        assign finalIndex mulIndex
        return finalIndex
