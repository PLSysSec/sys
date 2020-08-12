{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines Ord instances for various LLVM types
module LLVMAST.OrdInstances where


import           LLVM.AST
import           LLVM.AST.CallingConvention
import           LLVM.AST.InlineAssembly

deriving instance Ord Dialect
deriving instance Ord CallingConvention
deriving instance Ord InlineAssembly
deriving instance Ord Instruction
deriving instance Ord (Named Instruction)
deriving instance Ord Terminator
deriving instance Ord (Named Terminator)
deriving instance Ord BasicBlock

instance Ord Module where
  compare m1 m2 = compare (moduleSourceFileName m1) (moduleSourceFileName m2)
