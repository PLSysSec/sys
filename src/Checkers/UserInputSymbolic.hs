{-|

Symbolic counterpart to the user input static checker. Determines if it is possible
for a given variable to take a certain (malicious) value along a given path.

-}
module Checkers.UserInputSymbolic where
import           Control.Monad     (unless)
import           LLVM.AST
import           Symex.Name
import           Symex.Symex.Symex

userInputRefine :: Name -> Int -> Symex a ()
userInputRefine var val = do
  isTracked <- nameIsTracked var
  unless isTracked $ error "Variable of interest not tracked"
  varSym <- getDefinedName var
  varTy <- getTrackedNameType var
  valueSym <- numberConst varTy $ fromIntegral val
  isUge varTy varSym valueSym >>= assertBool
