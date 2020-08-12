-- | Helper module for translating predicates.
module Symex.IntegerPredicate where
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type
import           Prelude                   hiding (Ordering (..))
import           Symex.Symex.Symex         as Sym

-- | Helper for translating LLVM integer comparison predicates to Boolector comparisons.
-- This is its own function and module because both constants symex and instruction symex
-- rely on it.
translatePredicate :: IntegerPredicate -- ^ Integer predicate
                   -> (Type -> Node -> Node -> Symex a Node) -- ^ Symbolic comparison function
translatePredicate predicate = case predicate of
                                 EQ  -> Sym.isEq
                                 NE  -> Sym.isNe
                                 UGT -> Sym.isUgt
                                 UGE -> Sym.isUge
                                 ULT -> Sym.isUlt
                                 ULE -> Sym.isUle
                                 SGT -> Sym.isSgt
                                 SGE -> Sym.isSge
                                 SLT -> Sym.isSlt
                                 SLE -> Sym.isSle
