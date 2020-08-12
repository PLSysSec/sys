-- | Symex utils
module Symex.Symex.Utils ( number
                         , wnumber
                         , number32
                         , wnumber32
                         , numberWidth
                         , concatNodes
                         , orNodes
                         , andNodes
                         , sumNodes
                         )
                             where
import qualified Boolector     as B
import           Control.Monad (foldM)
import           Data.Word     (Word32)

-- Making simple nodes

number :: (B.MonadBoolector m) => Integer -> m B.Node
number num = B.bitvecSort 64 >>= B.unsignedInt num

wnumber :: (B.MonadBoolector m) => Word32 -> m B.Node
wnumber = number . fromIntegral

number32 :: (B.MonadBoolector m) => Integer -> m B.Node
number32 num = B.bitvecSort 32 >>= B.unsignedInt num

wnumber32 :: (B.MonadBoolector m) => Word32 -> m B.Node
wnumber32 = number32 . fromIntegral

numberWidth :: (B.MonadBoolector m) => Word32 -> Integer -> m B.Node
numberWidth width num = B.bitvecSort (fromIntegral width) >>= B.unsignedInt num

-- Multi-node operations

foldN :: (B.MonadBoolector m)
      => (B.Node -> B.Node -> m B.Node) -- ^ Operation
      -> [B.Node] -- ^ Nodes
      -> m B.Node -- ^ Result
foldN _ [node] = return node
foldN op nodes = foldM op (head nodes) (tail nodes)

concatNodes :: (B.MonadBoolector m) => [B.Node] -> m B.Node
concatNodes = foldN B.concat

orNodes :: (B.MonadBoolector m) => [B.Node] -> m B.Node
orNodes = foldN B.or

andNodes :: (B.MonadBoolector m) => [B.Node] -> m B.Node
andNodes = foldN B.and

sumNodes :: (B.MonadBoolector m) => [B.Node] -> m B.Node
sumNodes = foldN B.add
