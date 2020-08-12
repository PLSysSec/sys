{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-|

Wrapper around Boolector to protect users (ourselves) from confusing Boolector crashes.
It mostly wraps shift operations and array operations, since those are most error prone.
It also wraps the incremental solving we use for pointer location assignment.

-}
module Symex.Symex.Boolector ( -- * Shift operation wrappers
                               safeSll
                             , safeSrl
                             , safeSra
                             , safeRol
                             , safeRor
                             -- * Array operation wrappers
                             , makeMemory
                             , readWidth
                             , writeWidth
                             , readWidth64
                             , readWidth32
                             , writeWidth64
                             , writeWidth32
                             -- * Casting and padding
                             , castToWidth
                             , leftPadWithZeroes
                             , leftPadToWidth
                             , rightTruncToWidth
                             , rightPadWithZeroes
                             , rightPadToWidth
                             -- * Pointer allocations
                             , tryAllocationWith
                             , getAllocation
                             -- * The rest of Boolector with certain raw operations hidden:
                             -- shifts, functions and ops, arrays and ops, and quantifiers
                             , module Boolector
                             ) where
import           Boolector                  hiding (apply, exists, forall, funSort, read, rol, ror,
                                             setSymbol, sll, sra, srl, uf, write)
import qualified Boolector                  as B (array, arraySort, read, rol, ror, sll, sra, srl,
                                                  unsignedBvConst, write)
import           Control.Monad.State.Strict (unless)
import           Data.Word                  (Word32)
import           Prelude                    hiding (concat, read)

--
-- Shift operations
--

-- | Safe boolector shift operations
--
-- Boolector puts restrictions on the widths of the arguments to shift operations.
-- As of Boolector 3, the widths of both operands must be the same.
-- As of Boolectors < 3, the width of the first operand had to be a power of 2,
-- and the width of the second operand had to be log 2 of the first.
-- We no longer support the Boolector < 3 restriction
safeSll, safeSrl, safeSra, safeRol, safeRor :: MonadBoolector m => Node -> Node -> m Node
safeSll = shiftWrapper B.sll
safeSrl = shiftWrapper B.srl
safeSra = shiftWrapper B.sra
safeRol = shiftWrapper B.rol
safeRor = shiftWrapper B.ror

-- | Wrapper for boolector shift operations
shiftWrapper :: (MonadBoolector m)
             => (Node -> Node -> m Node) -- ^ Shift op
             -> Node -- ^ Thing to shift
             -> Node -- ^ Thing to shift by
             -> m Node -- ^ Result
shiftWrapper shiftOp toShift shiftVal = do
  castVal <- getWidth toShift >>= castToWidth shiftVal
  shiftOp toShift castVal

--
-- Arrays and array operations
--

-- | Make a new array representing memory
makeMemory :: (MonadBoolector m)
           => String -- ^ The name of the array
           -> Word
           -> Word
           -> m Node -- ^ The array itself
makeMemory name pointerSize alignSize = do
  pointerSort <- bitvecSort pointerSize
  cellSort <- bitvecSort alignSize
  arrSort <- B.arraySort pointerSort cellSort
  B.array arrSort name

-- | Read from an array with an index of a given width
readWidth :: (MonadBoolector m)
            => Word32 -- ^ Width
            -> Node -- ^ Array
            -> Node -- ^ Index
            -> m Node
readWidth width array index = do
  castIndex <- castToWidth index width
  B.read array castIndex

readWidth64 :: (MonadBoolector m)
            => Node -- ^ Array
            -> Node -- ^ Index
            -> m Node
readWidth64 array index = do
  castIndex <- castToWidth index 64
  B.read array castIndex

readWidth32 :: (MonadBoolector m)
            => Node -- ^ Array
            -> Node -- ^ Index
            -> m Node
readWidth32 array index = do
  castIndex <- castToWidth index 32
  B.read array castIndex

writeWidth64 :: (MonadBoolector m)
             => Node -- ^ Array
             -> Node -- ^ Index
             -> Node -- ^ Element
             -> m Node -- ^ Updated array
writeWidth64 array index elem = do
  castIndex <- castToWidth index 64
  B.write array castIndex elem

writeWidth32 :: (MonadBoolector m)
             => Node -- ^ Array
             -> Node -- ^ Index
             -> Node -- ^ Element
             -> m Node -- ^ Updated array
writeWidth32 array index elem = do
  castIndex <- castToWidth index 32
  B.write array castIndex elem

writeWidth :: (MonadBoolector m)
           => Word32 -- ^ Width
           -> Node -- ^ Array
           -> Node -- ^ Index
           -> Node -- ^ Element
           -> m Node -- ^ Updated array
writeWidth width array index elem = do
  castIndex <- castToWidth index width
  B.write array castIndex elem

-- | Cast a node to a new width
-- If the new width is larger, use unsigned extension
-- If the new width is smaller, slice
castToWidth :: (MonadBoolector m)
            => Node -- ^ Node to be cast
            -> Word32 -- ^ New width
            -> m Node -- ^ Result
castToWidth varToCast newWidth = do
  sort <- getSort varToCast
  let isBv = isBitvecSort sort || isBoolSort sort
  unless isBv $ error $ "Should never cast non-bitvector sort (" ++ show sort ++ ")"
  oldWidth' <- getWidth varToCast
  let oldWidth = fromIntegral oldWidth'
  case compare oldWidth newWidth of
    LT -> uext varToCast (newWidth - oldWidth)
    GT -> slice varToCast (newWidth - 1) 0
    _  -> return varToCast

leftPadWithZeroes :: (MonadBoolector m)
                  => Word32 -- ^ The number of zeros
                  -> Node -- ^ The thing to pad
                  -> m Node
leftPadWithZeroes numberOfZeroes node
    | numberOfZeroes == 0 = return node
    | numberOfZeroes > 0 = uext node numberOfZeroes
    | otherwise = error "Cannot left-pad to smaller width"

rightTruncToWidth :: (MonadBoolector m)
                 => Node -- ^ Node to trunc
                 -> Word32 -- ^ New width
                 -> m Node
rightTruncToWidth node newWidth = do
  curWidth <- getWidth node
  slice node (curWidth - 1) (curWidth - newWidth)

-- | Left-pad the node out to a given width
leftPadToWidth :: (MonadBoolector m)
               => Node -- ^ Node to pad
               -> Word32 -- ^ The new width
               -> m Node
leftPadToWidth node newWidth = do
  curWidth <- getWidth node
  leftPadWithZeroes (newWidth - curWidth) node

-- | Right-pad the node with a certain number (>= 0) of zeroes
-- eg rightPadWithZeroes node 6:
-- [node][0 0 0 0 0 0]
rightPadWithZeroes :: (MonadBoolector m)
                   => Word32 -- ^ The number of zeros to pad with
                   -> Node -- ^ The thing to pad
                   -> m Node
rightPadWithZeroes numberOfZeroes node
    | numberOfZeroes == 0 = return node
    | numberOfZeroes > 0 = do
        pad <- bitvecSort (fromIntegral numberOfZeroes) >>= zero
        concat node pad
    | otherwise = error "Cannot right-pad to smaller width"

-- | Right-pad the node out to a given width ( >= the current width)
-- eg rightPadToWidth node 6
-- [node][node's width - 6 * 0]
rightPadToWidth :: (MonadBoolector m)
               => Node -- ^ Node to pad
               -> Word32 -- ^ The new width
               -> m Node
rightPadToWidth node newWidth = do
  curWidth <- getWidth node
  rightPadWithZeroes (newWidth - curWidth) node

--
-- Pointer allocations
--

-- | Get the allocation for the given node
getAllocation :: (MonadBoolector m)
              => Node
              -> m Integer
getAllocation node = do
  result <- sat
  case result of
    Sat -> unsignedBvAssignment node
    _   -> error "Unsat or timed out"

-- | Try to assign a given location to a pointer
tryAllocationWith :: (MonadBoolector m)
                  => Node -- ^ Concrete location candidate
                  -> Node -- ^ Pointer whose location we want to determine
                  -> m Bool
tryAllocationWith locationCandidate pointer = do
  mConst <- B.unsignedBvConst pointer
  case mConst of
    Just{} -> return False
    _ -> do
      push 1
      eq locationCandidate pointer >>= assert
      result <- sat
      case result of
        Sat   -> return True
        Unsat -> pop 1 >> return False
        _     -> error "Timed out"


