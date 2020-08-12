{-|

Given information from the static checker in ConcreteOOBStatic.hs, this symbolic
checker determines if an out-of-bounds access of a given amount is possible and
reachable. The checker runs after Sys has symbolically executed a whole possibly
buggy path.

-}
module Checkers.ConcreteOOBSymbolic where
import           Control.Monad     (unless)
import           LLVM.AST
import           Symex.Name
import           Symex.Symex.Symex

-- | Determine if a given out-of-bounds access is possible and reachable.
negRefine :: (Either Name Integer) -> Integer -> Symex a ()
-- Out-of-bounds by a constant, so all we need to do is make sure that the
-- path is satisfiable: we already know that the access is OOB
negRefine (Right _) _ = return ()
-- Out-of-bounds by a variable: we need to make sure that the variable 'name' can equal
-- the out-of-bounds 'amt'.
negRefine (Left name) amt = do
  isTracked <- nameIsTracked name
  unless isTracked $ error "Var of concern not tracked"
  indexSym <- getDefinedName name
  castIndexSym <- castToWidth indexSym 64
  amountSym <- numberConst (IntegerType 64) amt
  -- Make sure the possibly out-of-bounds variable can be equal to the out-of-bounds amount.
  isEq (IntegerType 64) amountSym castIndexSym >>= assertBool


