-- | Implements symbolic memory operations (e.g., symbolic load, symbolic store).
module Symex.Symex.Memory ( -- * Loading and storing
                            load
                          , loadBits
                          , loadShadow
                          , loadShadowBits
                          , store
                          , storeBits
                          , setShadow
                          , setShadowBits
                          , initShadow
                            -- * Setting bits in structures
                            -- (This is not eventually exposed outside of the Symex dir)
                          , getBitsFrom
                          , setBitsTo
                            -- * Setting pointer locations
                          , stackAllocatePointer
                          , heapAllocatePointer
                          , heapAllocateMaybeNullPointer
                          , allocPointer
                          ) where
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.State.Strict
import qualified Data.Set                   as S
import           Data.Word                  (Word32)
import           InternalIR.PathInfo
import           LLVM.AST                   hiding (element, index, mask, value)
import           LLVM.AST.AddrSpace
import           Prelude                    hiding (null, reads)
import qualified Symex.Symex.Boolector      as B
import           Symex.Symex.SymexState     hiding (sizes)
import           Symex.Symex.Utils
import           Symex.Symex.Variable

-- | Load something of refTy's size from addressSym in memory
load :: B.Node
     -> Type
     -> Symex a B.Node
load addressSym refTy = loadWrapper addressSym refTy False False

-- | Load a certain number of bits from addressSym in memory
loadBits :: B.Node
         -> Int
         -> Symex a B.Node
loadBits addressSym bits = loadWrapper addressSym (IntegerType $ fromIntegral bits) False True

-- | Load something of a given type from addressSym in shadow memory
loadShadow :: B.Node
           -> Type
           -> Symex a B.Node
loadShadow addressSym refTy = loadWrapper addressSym refTy True False

loadShadowBits :: B.Node
               -> Int
               -> Symex a B.Node
loadShadowBits addressSym bits = loadShadow addressSym (IntegerType $ fromIntegral bits)

-- | Load from a pointer address
loadWrapper :: B.Node -- ^ Address to load from
            -> Type -- ^ The type of the stored thing
            -> Bool -- ^ Are we getting the thing from shadow memory?
            -> Bool -- ^ Should we load raw bits?
            -> Symex a B.Node -- ^ The result of the load
loadWrapper addressSym referentType isShadow isBits = do
  undefinedPointer addressSym referentType
  -- Setup
  pointerWidth <- pointerSize `liftM` get
  cellWidth <- cellSize `liftM` get
  cellWidthSym <- numberWidth pointerWidth $ fromIntegral cellWidth
  width <- numberWidth pointerWidth $ fromIntegral cellWidth

  -- Figure out which memory location the pointer corresponds to.
  -- Eg: pointer 64 corresponds to memory cell 1, pointer 128 corresponds to 2, etc
  pointerStart <- B.udiv addressSym width
  readSize <- if isBits
              then return $ case referentType of
                              IntegerType bits -> bits
                              _ -> error "Bits must be expressed as integer widths"
              else sizeOf referentType

  -- We may be reading across block bounaries if the cell size is more than 8
  let blocks = readSize `div` cellWidth
      blocks' = if blocks == 0 then 1 else blocks
      blocksToRead = if cellWidth > 8
                     then blocks' + 1
                     else blocks'

  when (blocksToRead > 2000) $ error "Load is too large"

  currentMemory <- curMemVer

  -- Read the contents of memory at all the blocks
  reads <- forM [0..blocksToRead - 1] $ \offset -> do
    pointer <- numberWidth pointerWidth (fromIntegral offset) >>= B.add pointerStart
    B.readWidth pointerWidth currentMemory pointer

  concatRead <- concatNodes reads
  readStart <- B.urem addressSym cellWidthSym
  getBitsFrom concatRead readSize readStart

  where curMemVer = if isShadow then currentShadowVersion else currentMemoryVersion

-- | Store a value to a pointer address
store :: B.Node -- ^ Address
      -> B.Node -- ^ Value to store
      -> Type -- ^ Type of the stored thing
      -> Symex a ()
store addr val ty = do
  storeWrapper addr val ty False False
  shouldShadow <- useShadowMem
  when shouldShadow $ setShadow addr ty

-- | Store the given bits to a pointer address
storeBits :: B.Node
          -> B.Node
          -> Integer
          -> Symex a ()
storeBits addr value bits = do
  storeWrapper addr value (IntegerType $ fromIntegral bits) False True
  shouldShadow <- useShadowMem
  bitSort <- B.bitvecSort $ fromIntegral bits
  shadowVal <- B.ones bitSort
  when shouldShadow $ storeWrapper addr shadowVal (IntegerType $ fromIntegral bits) True True

-- | Set the bits at addr in shadow memory
setShadow :: B.Node
          -> Type
          -> Symex a ()
setShadow addr ty = do
  shadowVal <- typeToSort ty >>= B.ones
  storeWrapper addr shadowVal ty True False

-- | Set a certain number of bits in shadow memory
setShadowBits :: B.Node
              -> Int
              -> Symex a ()
setShadowBits addr bits = setShadow addr (IntegerType $ fromIntegral bits)

-- | Initialize addr in shadow memory
initShadow :: B.Node
           -> Type
           -> Symex a ()
initShadow addr ty = do
  shadowVal <- typeToSort ty >>= B.zero
  storeWrapper addr shadowVal ty True False

-- | Store a value to a pointer address.
storeWrapper :: B.Node -- ^ Address
             -> B.Node -- ^ Value to store
             -> Type -- ^ The type of the stored thing
             -> Bool -- ^ Are we storing to shadow memory or normal memory
             -> Bool -- ^ Are we storing raw bits or a type?
             -> Symex a ()
storeWrapper unadjustedAddressSym valueSym valueType isShadow shouldStoreBits = do
  undefinedPointer unadjustedAddressSym valueType
  -- Figure out how many blocks we need
  pointerWidth <- pointerSize `liftM` get
  cellWidth <- cellSize `liftM` get
  cellWidthSym <- numberWidth pointerWidth $ fromIntegral cellWidth
  width <- numberWidth pointerWidth $ fromIntegral cellWidth
  addressSym <- B.udiv unadjustedAddressSym width

  writeSize <- if shouldStoreBits
               then return $ case valueType of
                      IntegerType bits -> fromIntegral bits
                      _ -> error "Must have an integer with the specified bitwidth"
               else sizeOf valueType

  when (writeSize `mod` 8 /= 0) $ error $ unwords ["Unaligned type size:"
                                                  , show valueType
                                                  , show writeSize

                                                    ]
  -- We may be reading across block bounaries if the cell size is more than 8
  let blocks = writeSize `div` cellWidth
      blocks' = if blocks == 0 then 1 else blocks
      blocksToRead = if cellWidth > 8
                     then blocks' + 1
                     else blocks'

  when (blocksToRead > 2000) $ error "Write too large"

  readStart <- B.urem unadjustedAddressSym cellWidthSym
  currentMemory <- curMemVer

  -- Read the contents of memory at all the blocks
  reads <- forM [0..blocksToRead - 1] $ \offset -> do
    pointer <- numberWidth pointerWidth (fromIntegral offset) >>= B.add addressSym
    B.readWidth pointerWidth currentMemory pointer
  toWrite <- concatNodes reads

  -- Write the relevant bits in the read
  write <- setBitsTo valueSym toWrite readStart

  -- Write the updated bits back to memory
  -- Write from high bits to low bits as the pointer values increase:
  -- ptr:   1        2        3      4
  -- val: [high][high mid][low mid][last]

  forM_ [0..blocksToRead - 1] $ \offset -> do
    pointer <- numberWidth pointerWidth (fromIntegral offset) >>= B.add addressSym
    let sliceStart = blocksToRead * cellWidth - (offset * cellWidth) - 1
        sliceEnd = sliceStart - cellWidth + 1
    writeSlice <- B.slice write sliceStart sliceEnd
    currentMemory' <- curMemVer
    newMemory' <- nextMemVer
    B.writeWidth pointerWidth currentMemory' pointer writeSlice >>= assign newMemory'

  where curMemVer = if isShadow then currentShadowVersion else currentMemoryVersion
        nextMemVer = if isShadow then nextShadowVersion else nextMemoryVersion

--
-- Setting and getting bits from bitvectors
--

-- | Get a given number of bits from a structure starting from a given symbolic index
getBitsFrom :: B.Node -- ^ In this structure
            -> Word32 -- ^ How large of a read
            -> B.Node -- ^ Starting from this index
            -> Symex a B.Node
getBitsFrom structure width index = do
  castIndex <- B.castToWidth index 64
  structureWidth <- B.getWidth structure
  -- Shift structure so the start of the element is the high bit
  elemAtHigh <- B.safeSll structure castIndex
  -- Slice from the high bit to the width of the element
  let elemStart = structureWidth - 1
      elemEnd = structureWidth - width
  B.slice elemAtHigh elemStart elemEnd

-- | Set a given number of bits in a structure starting from a given symbolic index
setBitsTo :: B.Node -- ^ Set to this
          -> B.Node -- ^ In this structure
          -> B.Node -- ^ Starting from this index
          -> Symex a B.Node -- ^ result
setBitsTo element structure index = do
  castIndex <- B.castToWidth index 64
  -- Information we will need later
  structureWidth <- B.getWidth structure
  elementWidth <- B.getWidth element
  let widthDifference = structureWidth - elementWidth

  if widthDifference == 0
  -- Setting every bit is just the same as returning the element
  then return element
  -- Otherwise we have to change some bits while preserving others
  else do

  -- struct: 1001..01011...1011
  -- mask:   1111..00000...1111
  ----------------------------- AND
  -- res:    1001..00000...1011
  -- elem:   0000..10110...0000
  ----------------------------- OR
  -- final:  1001..10110...1011

    -- Consturct *mask*:
    -- (0) Make [1 repeat width(element)][0 repeat width(structure - element0]
    ones <- B.getSort element >>= B.ones
    zeros <- B.bitvecSort (fromIntegral widthDifference) >>= B.zero
    preShiftMask <- B.concat ones zeros
    -- (1) Right shift to start at the correct index
    preNegMask <- B.safeSrl preShiftMask castIndex
    -- (2) Bitwise negate the whole thing
    mask <- B.not preNegMask

    -- And the struct with the mask
    res <- B.and mask structure

    -- Construct the *padded elemnt*:
    -- (0) Make [element][0 repeat width(structure - element)]
    preShiftElem <- B.concat element zeros
    -- (2) Right shift to start at the correct index
    finalElem <- B.safeSrl preShiftElem castIndex

    -- Or the two together!
    B.or finalElem res

--
-- Pointer location assignment
--

-- Currently unused
_strictAllocationOn :: Symex a Bool
_strictAllocationOn = strictAllocation `liftM` get

-- | Have we already determined a concrete location for the pointer?
pointerAlreadyAllocd :: B.Node -> Symex a Bool
pointerAlreadyAllocd pointer = do
  allocd <- alreadyAllocd `liftM` get
  return $ S.member pointer allocd

undefinedPointer :: B.Node
                 -> Type
                 -> Symex a ()
undefinedPointer var refTy = do
  shouldAllocate <- strictAllocation `liftM` get
  when shouldAllocate $ do
    allocd <- pointerAlreadyAllocd var
    unless allocd $ do
      ptr <- heapAllocatePointer refTy
      void $ B.tryAllocationWith ptr var
      allocPointer var

-- | Stack allocation: we know the size of the type
stackAllocatePointer :: Type -> Symex a B.Node
stackAllocatePointer pointeeType = do
  shouldAllocate <- strictAllocation `liftM` get
  if shouldAllocate
  then do
    pointeeSize <- sizeOf pointeeType
    nextPointer pointeeSize
  else freshVar $ PointerType pointeeType $ AddrSpace 0

-- | Heap allocate: we know the size of the type but not how many of them were allocated
heapAllocatePointer :: Type -> Symex a B.Node
heapAllocatePointer pointeeType = do
  shouldAllocate <- strictAllocation `liftM` get
  if shouldAllocate
  then do
    pointeeSize <- maxSizeOf pointeeType
    -- This size is meant to ensure that no combination of GEPs can ever smash
    -- into the pointer allocated right after its base.
    jumpToNextPointer <- heapAllocSize `liftM` pathInfo `liftM` get
    let size = pointeeSize * (fromIntegral jumpToNextPointer)
    nextPointer size
  else freshVar $ PointerType pointeeType $ AddrSpace 0

-- | Heap allocate a pointer that can either be null or of a certain size
heapAllocateMaybeNullPointer :: B.Node -> Type -> Symex a ()
heapAllocateMaybeNullPointer node pointeeType = do
  shouldAllocate <- strictAllocation `liftM` get
  when shouldAllocate $ do
    pointerWidth <- pointerSize `liftM` get
    pointersCanAlias <- canAlias `liftM` get
    possibleLocation <- heapAllocatePointer pointeeType
    null <- numberWidth pointerWidth 0
    pointerIsLocation <- B.eq node possibleLocation
    pointerIsNull <- B.eq node null
    pointerIsLocationOrNull <- B.or pointerIsLocation pointerIsNull
    if pointersCanAlias
    then do
      aliasLoc <- numberWidth pointerWidth 64
      pointerIsAliased <- B.eq node aliasLoc
      B.or pointerIsLocationOrNull pointerIsAliased >>= assert
    else assert pointerIsLocationOrNull
    allocPointer node

-- | Note that we have determined a concrete location for the pointer
allocPointer :: B.Node -> Symex a ()
allocPointer pointer = do
  s0 <- get
  put $ s0 { alreadyAllocd = S.insert pointer $ alreadyAllocd s0 }

-- | Bump up the pointer counter
nextPointer :: Word32 -> Symex a B.Node
nextPointer size = do
  s0 <- get
  -- Double check alignment: we shouldn't be allocating any oddly-sized things
  unless (size `mod` 8 == 0) $ error $ unwords [ "Unaligned size!"
                                               , "Pointee size:"
                                               , show size
                                               ]
  let memRegionEnd = curPointer s0

  -- Neither should the current pointer location be at a weird spot
  unless (memRegionEnd `mod` 8 == 0) $ error $ unwords ["Memory unlaigned:"
                                                       , show memRegionEnd
                                                       ]

  let memCellSize = cellSize s0
      toAdd = if size < memCellSize then memCellSize else size
      newMemRegionEnd = memRegionEnd + toAdd

  put $ s0 { curPointer = newMemRegionEnd }
  pointerWidth <- getPointerSize
  numberWidth pointerWidth $ fromIntegral memRegionEnd
