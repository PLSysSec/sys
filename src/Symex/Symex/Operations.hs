{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-|

This module implements most of the Sys DSL.

First, it includes some setup functions (e.g., figuring out how big pointers should be).
Then, it implements functions for creating aggregates and getting and setting elements, as
well as a simple GEP operation. Next, it implements vector operations so that it can make
all binary operations, etc, polymorhpic over both vector and scalar types. Finally, it
implements polymorphic wrappers around LLVM-style operations like add, sub, mul, etc.

-}
module Symex.Symex.Operations ( -- * Calculations about the initial state
                                initState
                              -- * Constants
                              , assertUnsat
                              , assertBool
                              , falseConst
                              , trueConst
                              , numberConst
                              , zeroConst
                              , oneConst
                              , allOnesConst
                              , shadowSetConst
                              -- * Indecies
                              , indexFromWord
                              -- * Pointers
                              , stackAllocatePointer
                              , heapAllocatePointer
                              -- * Aggregates
                              , aggregate
                              , getElement
                              , setElement
                              , doGep
                              -- * Vector operation
                              , shuffleVectorOp
                              -- * Wrappers for Boolector operations.
                              -- We do this because we pad many types for alignment,
                              -- so its important to automatically take the padding off
                              -- and put the padding back on
                              -- ** Selection op
                              , selectOp
                              -- ** Comparison ops
                              , isEq
                              , isNe
                              , isUgt
                              , isUge
                              , isUlt
                              , isUle
                              , isSgt
                              , isSge
                              , isSlt
                              , isSle
                              -- ** Cast ops
                              , CastTo(..)
                              , CastKind(..)
                              , castOp
                              , B.castToWidth
                              -- ** Binary ops
                              , addOp
                              , saddOp -- Overflow
                              , uaddOp -- Overflow
                              , subOp
                              , ssubOp -- Overflow
                              , usubOp -- Overflow
                              , mulOp
                              , smulOp -- Overflow
                              , umulOp -- Overflow
                              , udivOp
                              , sdivOp
                              , uremOp
                              , sremOp
                              , shlOp
                              , lshrOp
                              , ashrOp
                              , andOp
                              , orOp
                              , xorOp
                              , nandOp
                              -- ** Other ops 
                              , allBitsSet
                              , anyBitsSet
                              , noBitsSet
                              ) where

import           Control.Monad              (zipWithM)
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust)
import           Data.Maybe                 (isNothing)
import           Data.Word                  (Word32)
import           InternalIR.PathInfo
import           LLVM.AST.DataLayout
import           LLVM.AST.Type              hiding (void)
import qualified Symex.Symex.Boolector      as B
import           Symex.Symex.Memory
import           Symex.Symex.SymexState
import           Symex.Symex.Utils
import           Symex.Symex.Variable

--
-- State initialization
--

-- | Figure out how big the pointers in a given LLVM file are
calcPointerSize :: DataLayout -> Word32
calcPointerSize dl = let ptrSizes = map fst $ M.elems $ pointerLayouts dl
                         allSame = filter (== head ptrSizes) $ tail ptrSizes
                     in if null allSame
                        then head ptrSizes
                        else error "Don't support different pointer sizes at once"

-- | Figure out what cell size to use. Heuristic, may want to do something smarter
calcCellSize :: DataLayout -> Word32
calcCellSize dl =
  -- We want to match the aggregate minimum size or we'll be reading blocks
  -- across bounaries all the time. This is our upper bound on cell size
  let aggrMin = getMinAlign $ aggregateLayout dl
  -- Same with the pointer size. Keep a running minimum cell size
      pRequiredCells = catMaybes $ map requiresCell $ M.elems $ pointerLayouts dl
      pMin = if null pRequiredCells then aggrMin else minimum (aggrMin:pRequiredCells)
  -- Same with integer size. Continue the running minimum cell size count
      iRequiredCells = catMaybes $ map requiresCell' $ M.toList $ typeLayouts dl
      iMin = if null iRequiredCells then pMin else minimum (pMin:iRequiredCells)
  in iMin
  where requiresCell' ((VectorAlign, sz), ai)  = requiresCell (sz, ai)
        requiresCell' ((IntegerAlign, sz), ai) = requiresCell (sz, ai)
        requiresCell' _                        = Nothing
        requiresCell (size, ai) = let minAlign = getMinAlign ai
                                  in if minAlign < size then Just minAlign else Nothing
        getMinAlign ai = minimum [getPref ai, getAllowed ai]
        getPref (AlignmentInfo _ pref) = if pref == 0 then 64 else pref
        getAllowed (AlignmentInfo allowed _) = if allowed == 0 then 64 else allowed

-- | Initialize the symex state
initState :: Bool -> Bool -> Bool -> Bool -> Integer -> PathInfo -> Symex a ()
initState strictAlloc argsCanAlias shadow oob timeout info = do
  s0 <- get
  let mdl = programDataLayout info
      dl = if isNothing mdl then memLayout s0 else fromJust mdl
  put $ blankState { pathInfo = info
                   , memLayout = dl
                   , pointerSize = calcPointerSize dl
                   , cellSize = calcCellSize dl
                   , canAlias = argsCanAlias
                   , shadowMem = shadow
                   , oobAllowed = oob
                   , strictAllocation = strictAlloc
                   , solverTimeout = timeout
                   }
  void $ nextMemoryVersion
  void $ nextShadowVersion
  unless argsCanAlias $ do
    let args = M.toList $ parameterTypes info
    aggregates <- filterM (isAggregateTy `liftM` snd) args
    pointers <- filterM (isPointerTy `liftM` snd) args
    -- Assume that pointers can't alias and allocate them to concrete locations
    forM_ pointers $ \(varName, ty) -> do
      refTy <- referentTy ty
      var <- getVariable varName ty
      heapAllocateMaybeNullPointer var refTy
    -- Assume that aggregates can't alias and assert that they are not equal
    -- This implicitly prevents their pointers from being allowed to alias
    forM_ aggregates $ \(varName, ty) ->
        forM_ aggregates $ \(varName2, ty2) -> when (ty == ty2) $
          unless (varName == varName2) $ do
            var1 <- getVariable varName ty
            var2 <- getVariable varName2 ty2
            B.ne var1 var2 >>= assert

--
-- Aggregate creation, getting and setting elements
--

assertBool :: B.Node -> Symex a ()
assertBool boolNode = B.castToWidth boolNode 1 >>= assert

-- | Make the whole equation unsatisfiable
assertUnsat :: Symex a ()
assertUnsat = B.false >>= assert

-- | False (note that this will be 8 bits since we pad bools to 8)
falseConst :: Symex a B.Node
falseConst = symexBoolSort >>= B.zero

-- | True (note that this will be 8 bits)
trueConst :: Symex a B.Node
trueConst = symexBoolSort >>= B.one

-- | A number constant (e.g., 5). Must have an integer type
numberConst :: Type -> Integer -> Symex a B.Node
numberConst ty val = typeToSort ty >>= B.unsignedInt val

-- | Zero constant
zeroConst :: Type -> Symex a B.Node
zeroConst ty = numberConst ty 0

-- | One constant
oneConst :: Type -> Symex a B.Node
oneConst ty = numberConst ty 1

-- | All ones (e.g., if bitwidth 8, 11111111)
allOnesConst :: Type -> Symex a B.Node
allOnesConst ty = typeToSort ty >>= B.ones

-- | Returns true if all bits are set
allBitsSet :: B.Node -> Symex a B.Node
allBitsSet = B.redand

-- | Returns true if all bits are set
anyBitsSet :: B.Node -> Symex a B.Node
anyBitsSet = B.redor

-- | Returns true if all bits are set
noBitsSet :: B.Node -> Symex a B.Node
noBitsSet n = do
  n' <- B.redor n
  ff <- B.false
  B.eq n' ff

-- | A standard 'set' constant for setting shadow memory. You can use your own
-- constants, too, but this comes built in
shadowSetConst :: Type -> Symex a B.Node
shadowSetConst ty =
  case ty of
    IntegerType bits -> do
      typeSize <- sizeOf ty
      if bits == typeSize
      then allOnesConst ty
      else do
        allOnes <- B.bitvecSort (fromIntegral bits) >>= B.ones
        B.rightPadToWidth allOnes typeSize
    PointerType{} -> allOnesConst ty
    VectorType numElems elemTy -> do
      shadowElem <- shadowSetConst elemTy
      B.repeat shadowElem numElems
    StructureType _ elemTys -> do
      shadowElems <- mapM shadowSetConst elemTys
      aggregate ty shadowElems
    ArrayType numElems elemTy -> do
      shadowElem <- shadowSetConst elemTy
      B.repeat shadowElem $ fromIntegral numElems
    NamedTypeReference{} -> allOnesConst ty
    _ -> error "Do not support non-integer types rn"

-- | Internal function for our bool sort (different from Boolectors width-1 bit bool sort)
symexBoolSort :: Symex a B.Sort
symexBoolSort = typeToSort $ IntegerType 1

--
-- Making indecies from the LLVM-hs interface (where indecies can sometimes be Word32s)
--

-- | Given a Word32, get the node representing that index
indexFromWord :: Word32 -> Symex a B.Node
indexFromWord = wnumber

--
-- Aggregates
--

-- | Get an aggregate of type ty initialized to element values elems.
-- We can't just smush the elements together blindly since there may be
-- padding between the elements themselves.
aggregate :: Type -> [B.Node] -> Symex a B.Node
aggregate _ [] = error "Cannot make node with no elements"
aggregate ty elems = do
  targetSize <- sizeOf ty
  offsets' <- offsetsOf ty
  let offsets = map fromIntegral offsets'
  when (length offsets /= length elems) $ error "Length of offsets and elements not equal"
  -- The width of element_0..element_n-1 is given by: offset(element_1) - offset(element_0)
  -- The width of element_n is given by: targetSize - offset(element_n)
  let firstWidths = zipWith (-) (tail offsets) offsets
      finalWidth = targetSize - last offsets
  elemWidth <- B.getWidth $ head elems
  -- If the element width is larger than the determined padded widths,
  -- we have a vector situation
  isVector <- isVectorTy ty
  if isVector && elemWidth > finalWidth
  then do
    padded <- zipWithM B.castToWidth elems $ firstWidths ++ [finalWidth]
    concatNodes padded
  else do
    padded <- zipWithM B.rightPadToWidth elems $ firstWidths ++ [finalWidth]
    concatNodes padded

-- | Get the offset of a series of indecies into a (possibly nested) structure type
-- For example:
-- offsetOfIndecies { i32, { i32, i8 } }, 1, 1 = 64
-- We calculate this by doing:
-- (1) Index of the first element in { i32, { i32, i8 } } = 32
-- (2) 32 + Index of the first element in { i32, i8 }
offsetOfIndecies :: Type -> [B.Node] -> Symex a B.Node
offsetOfIndecies ty inds = do
  pointerWidth <- getPointerSize
  numberWidth pointerWidth 0 >>= getOffset ty inds
  where
    getOffset _ [] curOffset = return curOffset
    getOffset ty (ind:inds) curOffset = do
      -- Get the offset of the current index ind in ty
      newOffset <- offsetOfIndex ind ty >>= B.add curOffset
      if null inds
      -- If there are no other indecies, we're done descending into the nested aggregates
      then return newOffset
      -- Otherwise, we have to determine what type to descend into next.
      -- For example, given type { i32, { i32, i8 } } and index 1,
      -- we want to determine that the next type is { i32, i8 }
      else do
        unnamedTy <- getUnnamedTy ty
        nextType <- case unnamedTy of
                      ArrayType _ elementType -> return elementType
                      VectorType _ elementType -> return elementType
                      -- Indecies into structs must be constants
                      -- (or the compiler wouldn't know what type to make the result)
                      StructureType _ elementTypes -> do
                         maybeIndex <- B.unsignedBvConst ind
                         case maybeIndex of
                           Just index -> return $ elementTypes !! fromIntegral index
                           Nothing    -> error "Cannot symbolically index into struct"
                      _ -> error "Not an aggregate type"
        getOffset nextType inds newOffset

-- | Struct[index] = element, return the updated struct
-- This function applies to any aggregate type: struct, array, or vector
setElement :: B.Node -- ^ Structure
           -> Type -- ^ Structure type
           -> B.Node -- ^ Element
           -> [B.Node] -- ^ Indecies
           -> Symex a B.Node
setElement struct v@VectorType{} element inds = do
  offset <- offsetOfIndecies v inds
  elemSize <- sizeOfVectorElem v
  castElem <- B.castToWidth element $ fromIntegral elemSize
  setBitsTo castElem struct offset
setElement struct structTy element inds = do
  offset <- offsetOfIndecies structTy inds
  setBitsTo element struct offset

-- | Return struct[index]
-- This function applies to any aggregate type: struct, array, or vector
getElement :: B.Node -- ^ Structure
           -> Type -- ^ Structure type
           -> Type -- ^ Element type
           -> [B.Node] -- ^ Indecies
           -> Symex a B.Node
getElement struct v@VectorType{} elemTy inds = do
  offset <- offsetOfIndecies v inds
  elemSize <- sizeOfVectorElem v
  bits <- getBitsFrom struct (fromIntegral elemSize) offset
  resultSize <- sizeOf elemTy
  B.castToWidth bits resultSize
getElement struct structTy elemTy inds = do
  offset <- offsetOfIndecies structTy inds
  elemSize <- sizeOf elemTy
  getBitsFrom struct elemSize offset

-- | Perform a GEP operation:
-- The first argument is always a type used as the basis for the calculations. The second
-- argument is always a pointer or a vector of pointers, and is the base address to start
-- from. The remaining arguments are indices that indicate which of the elements of
-- the aggregate object are indexed. The interpretation of each index is dependent on
-- the type being indexed into. The first index always indexes the pointer value given as
-- the second argument, the second index indexes a value of the type pointed to
-- (not necessarily the value directly pointed to, since the first index can be non-zero),
-- etc. The first type indexed into must be a pointer value, subsequent types can be
-- arrays, vectors, and structs. Note that subsequent types being indexed into can never
-- be pointers, since that would require loading the pointer before continuing calculation.
--
-- The type of each index argument depends on the type it is indexing into. When indexing
-- into a (optionally packed) structure, only i32 integer constants are allowed (when using
-- a vector of indices they must all be the same i32 integer constant). When indexing
-- into an array, pointer or vector, integers of any width are allowed, and they are not
-- required to be constant. These integers are treated as signed values where relevant.
doGep :: B.Node -- ^ LHS result of the gep
      -> B.Node -- ^ Address from which to start
      -> Type -- ^ Pointee structure type
      -> [B.Node] -- ^ Indecies
      -> Symex a B.Node
doGep lhs startAddr pointeeTy (ind:inds) = do
  pointerWidth <- getPointerSize
  -- When we're in a strict allocation mode, make sure
  -- the base pointer cannot be null. If it *is* null, this will allows
  -- the GEP to crash into memory.
  strictAllocation <- strictAllocationOn
  when strictAllocation $ numberWidth pointerWidth 0 >>= B.ugt startAddr >>= assert
  -- Zeroth, make sure the index is not going to crash into any other heap allocated mem
  arrayIndex <- B.castToWidth ind pointerWidth
  allowOOBs <- oobsOk
  unless allowOOBs $ do
    maxJump <- maxJumpSize >>= numberWidth pointerWidth . fromIntegral
    isUge (IntegerType pointerWidth) maxJump arrayIndex >>= assertBool
  -- First, do the array jump
  pointeeSize <- sizeOf pointeeTy
  arrayJump <- numberWidth (fromIntegral pointerWidth) (fromIntegral pointeeSize) >>=
               B.mul arrayIndex
  -- Then, do the inner indexing
  offsetInStruct <- offsetOfIndecies pointeeTy inds
  result <- B.add startAddr arrayJump >>= B.add offsetInStruct
  assign lhs result
  -- The result is defined by the base pointer, so it is constrained by definition
  allocPointer lhs
  return result
doGep _ _ _ _ = error "Malformed gep instruction"

--
-- Vector element extractor and combiner.
-- To do vector operations, we need to split the vector into its constituent parts,
-- do an operation, and then recombine the vector into a single bitvector
--

-- | Get all the elements in a vector as seperate bitvectors
-- We use this as a helper for all the vector operations
seperateVectorElements :: B.Node -- ^ Vector
                       -> Type -- ^ Vector type
                       -> Symex a [B.Node] -- ^ All the elements
seperateVectorElements vector v@(VectorType numElems elemTy) = do
  elemSize <- sizeOfVectorElem v >>= return . fromIntegral
  realSize <- sizeOf elemTy
  let vectorIndecies = reverse [1 .. numElems]
  forM vectorIndecies $ \highIndex -> do
    let highOffset = elemSize * highIndex
        lowOffset = highOffset - elemSize
    slicedElem <- B.slice vector (highOffset - 1) lowOffset
    B.castToWidth slicedElem realSize
seperateVectorElements _ _ = error "Can't get elements from non-vector"

-- | Recombine the elements of a vector into that vector
-- We use this as a helper for all the vector operations
recombineVectorElements :: [B.Node] -- ^ Vector elements
                        -> Type -- ^ Vector type
                        -> Symex a B.Node -- ^ Recombined vector
recombineVectorElements vectorElems vectorType = aggregate vectorType vectorElems

-- | Shuffle the elements of a vector
--
-- The first two operands of a ‘shufflevector’ instruction are vectors with the same type.
-- The third argument is a shuffle mask whose element type is always ‘i32’. The result
-- of the instruction is a vector whose length is the same as the shuffle mask and whose
-- element type is the same as the element type of the first two operands.
--
-- The shuffle mask operand is required to be a constant vector with either constant
-- integer or undef values.
--
-- The elements of the two input vectors are numbered from left to right across both of
-- the vectors. The shuffle mask operand specifies, for each element of the result vector,
-- which element of the two input vectors the result element gets.
shuffleVectorOp :: B.Node -- ^ Mask
                -> Type -- ^ Mask type
                -> B.Node -- ^ First operand
                -> Type -- ^ First operand type
                -> B.Node -- ^ Second operand
                -> Type -- ^ Second operand type
                -> Symex a B.Node -- ^ The shuffled vector
shuffleVectorOp mask maskTy oper1 ty1 oper2 ty2 = do
  -- Get the elements in the vector arguments
  elems1 <- seperateVectorElements oper1 ty1
  elems2 <- seperateVectorElements oper2 ty2
  maskElems <- seperateVectorElements mask maskTy
  -- Make an array that implicitly labels the input vectors "from left to right"
  let elementArr = elems1 ++ elems2
  -- Perform the shuffle
  shuffledElements <- mapM (shuffleGetElement elementArr) maskElems
  -- The type of the new vector will have the length of mask and the base type of the opers
  baseType <- baseType ty1
  vectorElems <- numElems maskTy
  -- Recombine into the correct type
  recombineVectorElements shuffledElements $ VectorType vectorElems $ fromJust baseType
  where
    shuffleGetElement :: [B.Node] -- ^ Array of all the vector elements
                      -> B.Node -- ^ Index
                      -> Symex a B.Node -- ^ Result
    shuffleGetElement [] _ = error "Cannot make zero-length vector"
    shuffleGetElement vectorElements index = do
      indexValue <-  B.unsignedBvConst index
      return $ case indexValue of
        -- The index value was undef so we can do whatever we want
        Nothing  -> head vectorElements
        Just idx' -> let idx = fromIntegral idx'
                     in if idx >= length vectorElements
                        -- We could get a garbage index if the mask itself is undef.
                        -- In this case, just return whatever again
                        then head vectorElements
                        -- Otherwise, the index is a value that corresponds to an
                        -- index in our array of vector elements
                        else vectorElements !! idx

--
-- Wrappers for boolector operations
--

-- | Perform a selection.
-- We wrap this in order to unpad the the condition node from i8 to i1
selectOp :: B.Node -- ^ Condition
         -> Type -- ^ Condition type
         -> Type -- ^ Branch type
         -> B.Node -- ^ True value
         -> B.Node -- ^ False value
         -> Symex a B.Node -- ^ Result
-- If the condition is an i1 and it evaluates to 1, the instruction returns the
-- first value argument; otherwise, it returns the second value argument.
selectOp cond (IntegerType 1) _ trueBr falseBr = do
  -- The cond is padded out to an i8, so we need to truncate it to an i1
  -- This is because the boolector condition type must be an i1
  truncCond <- B.castToWidth cond 1
  B.cond truncCond trueBr falseBr
-- If the condition is a vector of i1, then the value arguments must be vectors of the
-- same size, and the selection is done element by element.
selectOp cond ty@(VectorType numElems _) brTy@(VectorType _ baseTy) trueBr falseBr = do
  condElems <- seperateVectorElements cond ty
  elems1 <- seperateVectorElements trueBr brTy
  elems2 <- seperateVectorElements falseBr brTy
  result <- forM [0..numElems - 1] $ \index' -> do
    let index = fromIntegral index'
        condElem = condElems !! index
        elem1 = elems1 !! index
        elem2 = elems2 !! index
    selectOp condElem (IntegerType 1) baseTy elem1 elem2
  recombineVectorElements result brTy
selectOp _ _ _ _ _ = error "Invalid type for select operation"

--
-- Comparison operations
--

isEq, isNe, isUgt, isUge, isUlt, isUle, isSgt
        , isSge, isSlt, isSle :: Type -> B.Node -> B.Node -> Symex a B.Node
isEq  = doCmpOp B.eq
isNe  = doCmpOp B.ne
isUgt = doCmpOp B.ugt
isUge = doCmpOp B.ugte
isUlt = doCmpOp B.ult
isUle = doCmpOp B.ulte
isSgt = doCmpOp B.sgt
isSge = doCmpOp B.sgte
isSlt = doCmpOp B.slt
isSle = doCmpOp B.slte

-- | Perform a selection.
-- We wrap this in order to
-- (1) Account for vector comparison
-- (2) Pad the result of the comparison, since boolector returns an i1
-- (which we want to pad to an i8)
doCmpOp :: (B.Node -> B.Node -> Symex a B.Node) -- ^ Comparison operator
        -> Type -- ^ Type of result and operands
        -> B.Node -- ^ First operand
        -> B.Node -- ^ Second operand
        -> Symex a B.Node -- ^ Comparison result
doCmpOp op ty@(VectorType numElems elemTy) oper1 oper2 = do
  -- Do the comparison element-by-element
  elems1 <- seperateVectorElements oper1 ty
  elems2 <- seperateVectorElements oper2 ty
  result <- zipWithM (doCmpOp op elemTy) elems1 elems2
  recombineVectorElements result $ VectorType numElems $ IntegerType 1
doCmpOp op (IntegerType expectedSize) oper1 oper2 = do
  castOper1 <- B.castToWidth oper1 expectedSize
  castOper2 <- B.castToWidth oper2 expectedSize
  result <- op castOper1 castOper2
  -- Pad to the width of a boolean
  boolSize <- sizeOf $ IntegerType 1
  B.leftPadToWidth result boolSize
doCmpOp op _ oper1 oper2 = do
  result <- op oper1 oper2
  boolSize <- sizeOf $ IntegerType 1
  B.leftPadToWidth result boolSize

--
-- Binary operations
--

addOp, subOp, mulOp, udivOp, sdivOp, uremOp, sremOp, shlOp, lshrOp
         , ashrOp, andOp, orOp, xorOp, nandOp :: Type -> B.Node -> B.Node -> Symex a B.Node
addOp  = doBinOp B.add
subOp  = doBinOp B.sub
mulOp  = doBinOp B.mul
udivOp = doBinOp B.udiv
sdivOp = doBinOp B.sdiv
uremOp = doBinOp B.urem
sremOp = doBinOp B.srem
shlOp  = doBinOp B.safeSll
lshrOp = doBinOp B.safeSrl
ashrOp = doBinOp B.safeSra
andOp  = doBinOp B.and
orOp   = doBinOp B.or
xorOp  = doBinOp B.xor
nandOp = doBinOp B.nand

saddOp, uaddOp, ssubOp, usubOp, smulOp, umulOp :: Type -> B.Node -> B.Node -> Symex a B.Node
saddOp = doOverflowBinOp B.add
uaddOp = saddOp
ssubOp = doOverflowBinOp B.sub
usubOp = ssubOp
smulOp = doOverflowBinOp B.mul
umulOp = smulOp

-- | Perform an LLVM overflow intrinsic binop.
-- This function returns a { result, bool }, where result
-- is the result of the operation, and bool indicates whether the
-- operation overflowed.
-- Right now, we only support a limited selection of types and widths
-- for this intrinsic.
doOverflowBinOp :: (B.Node -> B.Node -> Symex a B.Node) -- ^ Binary operator
                -> Type -- ^ Type of the result and operatnds
                -> B.Node -- ^ First operand
                -> B.Node -- ^ Second operand
                -> Symex a B.Node
doOverflowBinOp op ty@(IntegerType width) oper1 oper2 = do
  -- Make wider versions of the arguments
  let overflowWidth = case width of
                        8  -> 16
                        16 -> 32
                        32 -> 64
                        64 -> 128
                        _  -> error "Unsupported width for overflow intrinsic"
  wide1 <- B.castToWidth oper1 overflowWidth
  wide2 <- B.castToWidth oper2 overflowWidth
  -- Get the result of the original args and the widened args.
  -- If the results are different, we have encountered overflow
  result <- op oper1 oper2
  castResult <- B.castToWidth result overflowWidth
  wideResult <- op wide1 wide2
  overflowed <- B.ne castResult wideResult
  paddedOverflowed <- B.castToWidth overflowed 8
  -- Build the result struct { result, did overflow }
  let returnType = StructureType False [ty, IntegerType 1]
  aggregate returnType [result, paddedOverflowed]
doOverflowBinOp _ _ _ _ = error "Unsupported arguments to overflow intrinsic"

-- | Perform a binop.
-- We wrap this in order to:
-- (1) Account for vector binops
-- (2) Unpad the operands...
-- (3) ... and re-pad the result.
doBinOp :: (B.Node -> B.Node -> Symex a B.Node) -- ^ Binary operator
        -> Type -- ^ Type of result and operands
        -> B.Node -- ^ First operand
        -> B.Node -- ^ Second operand
        -> Symex a B.Node -- ^ Binary operation result
doBinOp op ty@(IntegerType width) oper1 oper2 = do
  typeSize <- sizeOf ty
  let typeSize' = fromIntegral typeSize
  if typeSize' == width
  then op oper1 oper2
  -- If the type itself is not the same size as our representation, we have to
  -- cast to and from the representation size
  else do
    cast1 <- B.castToWidth oper1 width
    cast2 <- B.castToWidth oper2 width
    result <- op cast1 cast2
    B.castToWidth result typeSize'
-- Otherwise it must be a vector type
doBinOp op ty@(VectorType _ elemTy) oper1 oper2 = do
  -- Just do the operation element-by-element
  elems1 <- seperateVectorElements oper1 ty
  elems2 <- seperateVectorElements oper2 ty
  result <- zipWithM (doBinOp op elemTy) elems1 elems2
  recombineVectorElements result ty
doBinOp _ _ _ _ = error "Not a valid type for a binary operation"

-- | TODO: COME BACK TO THIS COMMENT
castOp :: CastTo -- ^ The type to cast to and the kind of cast to use
       -> B.Node -- ^ The value to cast
       -> Type   -- ^ Original type
       -> Symex a B.Node -- ^ The cast expression
castOp castType paddedVarToCast originalType =
  case originalType of
    -- Do an operation on each element of the vector
    (VectorType numElems vectorBaseType) | not $ isBitcast castType -> do
      (Just elemBaseType) <- baseType (toType castType)
      let elementCastTo = CastTo elemBaseType $ castKind castType
      elements <- seperateVectorElements paddedVarToCast originalType
      castElements <- mapM (\e -> castOp elementCastTo e vectorBaseType) elements
      recombineVectorElements castElements $ VectorType numElems elemBaseType
    -- Do an operation just on the variable itself
    _ -> do
      -- First, get rid of any padding on varToCast
      oldWidth <- case originalType of
                    -- We may have padded a weird-sized integer out to be a multiple of
                    -- 8 bits. We need to know the real width in order to do the cast
                    -- operation properly
                    IntegerType width -> return width
                    -- If the type isn't an integer, the only possible cast is a bitcast
                    -- anyway... so "unpadding" becomes nonsensical
                    _                 -> B.getWidth paddedVarToCast
      varToCast <- B.castToWidth paddedVarToCast oldWidth
      -- Next, figure out how much padding we need for the new size
      let newType = toType castType
      newPaddedWidth <- sizeOf newType
      -- Perform the actual cast
      case castKind castType of
        BitCastKind -> do
          -- If its a bitcast, the variables should also have the same padding,
          -- so we're going to do all of the checks on the padded variable before
          -- also returning the padded variable
          oldSort <- B.getSort paddedVarToCast
          newSort <- typeToSort newType
          unless (oldSort == newSort) $ error $ unwords ["Cannot bitcast into differnet sort"
                                                        , show originalType
                                                        , show newType
                                                        , show paddedVarToCast
                                                        ]
          let isBv = B.isBitvecSort newSort
          unless isBv $ error "Cannot bitcast to non-bitvector sort"
          return paddedVarToCast
        TruncKind -> case newType of
                       IntegerType newWidth -> if newWidth >= oldWidth
                                               then error "Can't trunc from smaller width"
                                               else do
                                                 -- Do the truncation
                                                 trunc <- B.slice varToCast (newWidth - 1) 0
                                                 -- Do the padding
                                                 B.leftPadToWidth trunc newPaddedWidth
                       _ -> error "Trunc expects an integer type"
        ZExtKind -> case newType of
                      -- Just left-pad, since that is an unsigned extension with
                      -- error checking anyway.
                      IntegerType _ -> B.leftPadToWidth varToCast newPaddedWidth
                      _             -> error "Zext expects an integer type"
        SExtKind -> case newType of
                      IntegerType newWidth -> if newWidth <= oldWidth
                                              then error "Can't ext from larger width"
                                              else do
                                                -- Do the sext seperately from the padding,
                                                -- since the padding is a uext
                                                sext <- B.sext varToCast (newWidth - oldWidth)
                                                -- Pad the result
                                                B.leftPadToWidth sext newPaddedWidth
                      _ -> error "Sext expects an integer type"
        PtrToIntKind -> do
          pointerWidth <- getPointerSize
          case newType of
            IntegerType newWidth -> case compare pointerWidth newWidth of
                                      GT -> do -- Do trunc and pad seperately
                                        trunc <- B.slice varToCast (newWidth - 1) 0
                                        B.leftPadToWidth trunc newPaddedWidth
                                      -- If it's an extension or just the same
                                      -- size, we pad it as normal, since it will
                                      -- not pad if it doesn't need to
                                      _ -> B.leftPadToWidth varToCast newPaddedWidth
            _ -> error "Cannot ptrtoint a non-pointer type"
        IntToPtrKind -> do
          pointerWidth <- getPointerSize
          case compare oldWidth pointerWidth of
            GT -> B.slice varToCast (pointerWidth - 1) 0
            _  -> B.leftPadToWidth varToCast pointerWidth
        UnsupportedKind -> error "Unsupported cast kind"
        -- Floating point truncation and extension: check semantics
        FPTruncKind -> do
          newWidth <- sizeOf newType
          if newWidth >= oldWidth
          then error "Can't fptrunc from smaller width"
          else do
            trunc <- B.slice varToCast (newWidth - 1) 0
            B.leftPadToWidth trunc newPaddedWidth
        FPExtKind -> B.leftPadToWidth varToCast newPaddedWidth
        -- The remaining possible kinds are:
        -- FPToUIKind, FPToSIKind, UIToFPKind, and SIToFPKind
        -- Since we dummy FP numbers as integers right now, we don't have to do
        -- casting in this function, just changing the bitwidth
        _ -> do
           newWidth <- sizeOf newType
           case compare oldWidth newWidth of
             GT -> B.slice varToCast (newWidth - 1) 0
             _  -> B.leftPadToWidth varToCast newWidth

data CastTo = CastTo { toType   :: Type
                     , castKind :: CastKind
                     }

isBitcastKind :: CastKind -> Bool
isBitcastKind BitCastKind = True
isBitcastKind _           = False

isBitcast :: CastTo -> Bool
isBitcast = isBitcastKind . castKind


data CastKind = TruncKind | ZExtKind | SExtKind | PtrToIntKind
              | IntToPtrKind | BitCastKind | UnsupportedKind
              | FPToUIKind | FPToSIKind | UIToFPKind | SIToFPKind
              | FPTruncKind | FPExtKind
              deriving (Eq, Ord, Show)
