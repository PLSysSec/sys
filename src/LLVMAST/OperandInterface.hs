module LLVMAST.OperandInterface where

import           LLVM.AST.Name             (Name)
import           LLVM.AST.Operand
import           LLVM.AST.Type
import           LLVM.AST.Typed
import           LLVMAST.ConstantInterface (getConstantInt, isNullConstant)

isNullOperand :: Operand -> Bool
isNullOperand (ConstantOperand c) = isNullConstant c
isNullOperand _                   = False

getOperandName :: Operand -> Maybe Name
getOperandName (LocalReference _ opname) = Just opname
getOperandName (ConstantOperand _)       = Nothing
getOperandName op                        = error $ "getOperandName: operand not handled: " ++ show op

getOperandIntConstant :: Operand -> Integer
getOperandIntConstant (ConstantOperand constant) = getConstantInt constant
getOperandIntConstant op = error $ "getOperandIntConstant: not a constant: " ++ show op

isConstantOperand :: Operand -> Bool
isConstantOperand ConstantOperand{} = True
isConstantOperand _                 = False

isConstant :: Operand -> Bool
isConstant = isConstantOperand

isLocalOperand :: Operand -> Bool
isLocalOperand LocalReference{} = True
isLocalOperand _                = False

getVectorBaseType :: Operand -> Type
getVectorBaseType operand =
  case typeOf operand of
    VectorType _ baseTy -> baseTy
    t                   -> error $ unwords [ "Can't get base type of"
                                             , show t
                                             ]
