{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module for translating LLVM operands into their Boolector variable counterparts.
module Symex.Operand ( getOperand
                     , trackOperand
                     ) where

import           Control.Monad       (void)
import           InternalIR.PathInfo (makeVarName)
import           LLVM.AST.Operand
import           Symex.Constant
import           Symex.Symex.Symex

-- | Get a variable used in an IR operation
getOperand :: Operand -> Symex a Node
getOperand (LocalReference ty llvmName) = getVariable (makeVarName llvmName) ty
getOperand (ConstantOperand constant)   = getConstant constant
getOperand _                            = error "getOperand: No metadata nodes"

-- | Start tracking an operand. There is no way to start tracking a variable name,
-- since we do not know a variable's type from its name
trackOperand :: Operand -> Symex a ()
trackOperand op = void $ getOperand op
