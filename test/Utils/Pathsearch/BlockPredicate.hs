{- | Simple API for writing predicates on blocks -}
module Utils.Pathsearch.BlockPredicate ( -- * Predicate DSL
                                         BlockPredicate(..)
                                       , isTrue
                                       , constPred
                                       , ifAny
                                       , ifAnyInstruction
                                       , ifTerminator
                                       , allPred
                                       ) where

import           LLVM.AST
import           LLVMAST.ASTInterface
import           InternalIR.ModuleInfo

-- | Quit early predicate
newtype BlockPredicate = BlockPredicate (ModuleInfo -> BasicBlock -> Bool)

instance Show BlockPredicate where
  show (BlockPredicate _) = "BlockPredicate"

-- | Constant predicate
constPred :: Bool -> BlockPredicate
constPred c = BlockPredicate $ \_ _ -> c

-- | Returns true if all predicate returns true
allPred :: [BlockPredicate] -> BlockPredicate
allPred preds = BlockPredicate $ \mInfo bb -> all (\(BlockPredicate f) -> f mInfo bb) preds

-- | Returns true if any predicate returns true
ifAny :: [BlockPredicate] -> BlockPredicate
ifAny preds = BlockPredicate $ \mInfo bb -> any (\(BlockPredicate f) -> f mInfo bb) preds

-- | Returns true if any predicate is true for any instruciton
ifAnyInstruction :: (ModuleInfo -> Instruction -> Bool) -> BlockPredicate
ifAnyInstruction ipred = BlockPredicate $ \mInfo bb -> any (ipred mInfo) $ map withoutName $ getBlockContents bb

-- | Returns true if the terminator predicate returns true
ifTerminator :: (ModuleInfo -> Terminator -> Bool) -> BlockPredicate
ifTerminator tpred = BlockPredicate $ \mInfo bb -> tpred mInfo $ withoutName $ getBlockTerminator bb

-- | Apply predicate to basic block.
isTrue :: BlockPredicate -> ModuleInfo -> BasicBlock -> Bool
isTrue (BlockPredicate p) mInfo bb = p mInfo bb
