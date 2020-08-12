module LLVMAST.ConstantInterface where
import           LLVM.AST.Constant

-- Plain queries about kinds of constants
isIntConstant :: Constant -> Bool
isIntConstant Int{} = True
isIntConstant _     = False

isFloatConstant :: Constant -> Bool
isFloatConstant Float{} = True
isFloatConstant _       = False

isNullConstant :: Constant -> Bool
isNullConstant Null{} = True
isNullConstant _      = False

isAggregateZeroConstant :: Constant -> Bool
isAggregateZeroConstant AggregateZero{} = True
isAggregateZeroConstant _               = False

isStructConstant :: Constant -> Bool
isStructConstant Struct{} = True
isStructConstant _        = False

isArrayConstant :: Constant -> Bool
isArrayConstant Array{} = True
isArrayConstant _       = False

isVectorConstant :: Constant -> Bool
isVectorConstant Vector{} = True
isVectorConstant _        = False

isUndefConstant :: Constant -> Bool
isUndefConstant Undef{} = True
isUndefConstant _       = False

isGlobalReferenceConstant :: Constant -> Bool
isGlobalReferenceConstant GlobalReference{} = True
isGlobalReferenceConstant _                 = False

getConstantInt :: Constant -> Integer
getConstantInt (Int _ value) = value
getConstantInt c             = error $ "getConstantInt: not an integer constant: " ++ show c

getConstantOperands :: Constant -> [Constant]
getConstantOperands constant =
  case constant of
    Struct _ _ opers                -> opers
    Array _ opers                   -> opers
    Vector opers                    -> opers
    Add _ _ oper1 oper2             -> [oper1, oper2]
    FAdd oper1 oper2                -> [oper1, oper2]
    Sub _ _ oper1 oper2             -> [oper1, oper2]
    FSub oper1 oper2                -> [oper1, oper2]
    Mul _ _ oper1 oper2             -> [oper1, oper2]
    FMul oper1 oper2                -> [oper1, oper2]
    UDiv _ oper1 oper2              -> [oper1, oper2]
    SDiv _ oper1 oper2              -> [oper1, oper2]
    FDiv oper1 oper2                -> [oper1, oper2]
    URem oper1 oper2                -> [oper1, oper2]
    SRem oper1 oper2                -> [oper1, oper2]
    FRem oper1 oper2                -> [oper1, oper2]
    Shl _ _ oper1 oper2             -> [oper1, oper2]
    LShr _ oper1 oper2              -> [oper1, oper2]
    AShr _ oper1 oper2              -> [oper1, oper2]
    And oper1 oper2                 -> [oper1, oper2]
    Or oper1 oper2                  -> [oper1, oper2]
    Xor oper1 oper2                 -> [oper1, oper2]
    GetElementPtr _ addr inds       -> addr:inds
    Trunc oper _                    -> [oper]
    ZExt oper _                     -> [oper]
    SExt oper _                     -> [oper]
    FPToUI oper _                   -> [oper]
    FPToSI oper _                   -> [oper]
    UIToFP oper _                   -> [oper]
    SIToFP oper _                   -> [oper]
    FPTrunc oper _                  -> [oper]
    FPExt oper _                    -> [oper]
    PtrToInt oper _                 -> [oper]
    IntToPtr oper _                 -> [oper]
    BitCast oper _                  -> [oper]
    AddrSpaceCast oper _            -> [oper]
    ICmp _ oper1 oper2              -> [oper1, oper2]
    FCmp _ oper1 oper2              -> [oper1, oper2]
    Select oper1 oper2 oper3        -> [oper1, oper2, oper3]
    ExtractElement oper1 oper2      -> [oper1, oper2]
    InsertElement oper1 oper2 oper3 -> [oper1, oper2, oper3]
    ShuffleVector oper1 oper2 oper3 -> [oper1, oper2, oper3]
    ExtractValue oper _             -> [oper]
    InsertValue oper1 oper2 _       -> [oper1, oper2]
    _                               -> []
