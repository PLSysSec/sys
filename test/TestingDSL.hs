{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module TestingDSL ( -- * Types
                    i1
                  , i8, i8', i8''
                  , i32, i32', i32''
                  , i64, i64', i64''
                  , void
                  -- * Operations
                  , nsw, nuw, nuw'nsw, nsw'nuw
                  , add, mul, icmp
                  , bitcast
                  , eq, ne, ugt, uge, ult, ule, sgt, sge, slt, sle
                  , alloca
                  , store
                  , load
                  , getelementptr
                  , call, tail'call
                  , ccc
                  , phi
                  -- ** Operands
                  , r, ci
                  -- * Symex helpers
                  , uname, uvar
                  ) where
import           LLVM.AST                   hiding (nsw, nuw)
import           LLVM.AST.AddrSpace
import           LLVM.AST.CallingConvention
import qualified LLVM.AST.Constant          as C
import qualified LLVM.AST.IntegerPredicate  as I
import           LLVM.AST.Typed
import           LLVM.Prelude               hiding (void)
import           Symex.Symex.Symex          (VarName)



uname :: (Integral a) => a -> Name
uname num = UnName $ fromIntegral num

uvar :: (Integral a) => a -> VarName
uvar = show . uname

void :: Type
void = VoidType

i1 :: Type
i1 = IntegerType 1

i8 :: Type
i8 = IntegerType 8

i8' :: Type
i8' = PointerType i8 (AddrSpace 0)

i8'' :: Type
i8'' = PointerType i8' (AddrSpace 0)

i32 :: Type
i32 = IntegerType 32

i32' :: Type
i32' = PointerType i32 (AddrSpace 0)

i32'' :: Type
i32'' = PointerType i32' (AddrSpace 0)

i64 :: Type
i64 = IntegerType 64

i64' :: Type
i64' = PointerType i64 (AddrSpace 0)

i64'' :: Type
i64'' = PointerType i64' (AddrSpace 0)

r :: Type -> ShortByteString -> Operand
r ty str = LocalReference ty (Name str)

ci :: Type -> Integer -> Operand
ci ty i = ConstantOperand $ C.Int (typeBits ty) i


data WrapMode = NSW | NUW | NUW'NSW | NSW'NUW
  deriving (Eq, Show)

hasMode :: WrapMode -> WrapMode -> Bool
hasMode _ NSW'NUW = True
hasMode _ NUW'NSW = True
hasMode x y       = x == y

class DSLBinOp a b where
  add :: WrapMode -> Type -> a -> b -> Instruction
  mul :: WrapMode -> Type -> a -> b -> Instruction
  icmp :: I.IntegerPredicate -> Type -> a -> b -> Instruction

instance DSLBinOp ShortByteString ShortByteString where
  add wm ty o1 o2 = Add (hasMode NSW wm) (hasMode NUW wm) (r ty o1) (r ty o2) []
  mul wm ty o1 o2 = Mul (hasMode NSW wm) (hasMode NUW wm) (r ty o1) (r ty o2) []
  icmp p ty o1 o2 = ICmp p (r ty o1) (r ty o2) []

instance DSLBinOp ShortByteString Integer where
  add wm ty o1 o2 = Add (hasMode NSW wm) (hasMode NUW wm) (r ty o1) (ci ty o2) []
  mul wm ty o1 o2 = Mul (hasMode NSW wm) (hasMode NUW wm) (r ty o1) (ci ty o2) []
  icmp p ty o1 o2 = ICmp p (r ty o1) (ci ty o2) []

instance DSLBinOp Integer ShortByteString where
  add wm ty o1 o2 = Add (hasMode NSW wm) (hasMode NUW wm) (ci ty o1) (r ty o2) []
  mul wm ty o1 o2 = Mul (hasMode NSW wm) (hasMode NUW wm) (ci ty o1) (r ty o2) []
  icmp p ty o1 o2 = ICmp p (ci ty o1) (r ty o2) []

class DSLUniOp a where
  bitcast :: Type -> a -> Type -> Instruction
  store   :: Type -> a -> ShortByteString -> Word32 -> Instruction

instance DSLUniOp ShortByteString where
  bitcast ty1 n ty2 = BitCast (r ty1 n) ty2 []
  store ty1 v addr al = Store { volatile = False
                              , address = r (PointerType ty1 (AddrSpace 0)) addr
                              , value = r ty1 v
                              , maybeAtomicity = Nothing
                              , alignment = al
                              , metadata = [] }

instance DSLUniOp Integer where
  bitcast ty1 i ty2 = BitCast (ci ty1 i) ty2 []
  store ty1 v addr al = Store { volatile = False
                              , address = r (PointerType ty1 (AddrSpace 0)) addr
                              , value = ci ty1 v
                              , maybeAtomicity = Nothing
                              , alignment = al
                              , metadata = [] }

alloca :: Type -> Word32 -> Instruction
alloca ty al = Alloca  ty Nothing al []

load :: Type -> ShortByteString -> Word32 -> Instruction
load ty1 addr al = Load { volatile = False
                        , address = r (PointerType ty1 (AddrSpace 0)) addr
                        , maybeAtomicity = Nothing
                        , alignment = al
                        , metadata = [] }

class DSLGetElementPtr a where
  getelementptr :: Bool -> Type -> ShortByteString -> [a] -> Instruction

instance DSLGetElementPtr (Type, ShortByteString) where
  getelementptr ib ty addr ids = GetElementPtr { inBounds = ib
                                               , address  = r (PointerType ty (AddrSpace 0)) addr
                                               , indices  = map (uncurry r) ids
                                               , metadata = []
                                               }

instance DSLGetElementPtr Operand where
  getelementptr ib ty addr ids = GetElementPtr { inBounds = ib
                                               , address  = r (PointerType ty (AddrSpace 0)) addr
                                               , indices  = ids
                                               , metadata = []
                                               }

class DSLCall a where
  tail'call :: CallingConvention -> Type -> ShortByteString -> [a] -> Instruction
  tail'call = call (Just Tail)
  call :: Maybe TailCallKind -> CallingConvention -> Type -> ShortByteString -> [a] -> Instruction

instance DSLCall (Type, ShortByteString) where
  call mt cc ty name args' = Call { tailCallKind = mt
                                  , callingConvention = cc
                                  , returnAttributes = []
                                  , function = Right $ ConstantOperand $ C.GlobalReference fType (Name name)
                                  , arguments = map (\(ty', n) -> (r ty' n, [])) args'
                                  , functionAttributes = []
                                  , metadata = [] }
    where fType = PointerType { pointerReferent = FunctionType { resultType = ty
                                                              , argumentTypes = map fst args'
                                                              , isVarArg = False }
                              , pointerAddrSpace = AddrSpace 0}

instance DSLCall Operand where
  call mt cc ty name args' = Call { tailCallKind = mt
                                  , callingConvention = cc
                                  , returnAttributes = []
                                  , function = Right $ ConstantOperand $ C.GlobalReference fType (Name name)
                                  , arguments = map (\o -> (o, [])) args'
                                  , functionAttributes = []
                                  , metadata = [] }
    where fType = PointerType { pointerReferent = FunctionType { resultType = ty
                                                                , argumentTypes = map typeOf args'
                                                                , isVarArg = False }
                              , pointerAddrSpace = AddrSpace 0}

class DSLPhi a where
  phi :: Type -> [(a, Name)] -> Instruction

instance DSLPhi Integer where
  phi ty ivs0 = Phi ty iv1 []
    where iv1 = map (\(o, n) -> (ci ty o, n)) ivs0

instance DSLPhi ShortByteString where
  phi ty ivs0 = Phi ty iv1 []
    where iv1 = map (\(o, n) -> (r ty o, n)) ivs0

instance DSLPhi Operand where
  phi ty ivs = Phi ty ivs []


ccc :: CallingConvention
ccc = C

eq  :: I.IntegerPredicate
eq  = I.EQ
ne  :: I.IntegerPredicate
ne  = I.NE
ugt :: I.IntegerPredicate
ugt = I.UGT
uge :: I.IntegerPredicate
uge = I.UGE
ult :: I.IntegerPredicate
ult = I.ULT
ule :: I.IntegerPredicate
ule = I.ULE
sgt :: I.IntegerPredicate
sgt = I.SGT
sge :: I.IntegerPredicate
sge = I.SGE
slt :: I.IntegerPredicate
slt = I.SLT
sle :: I.IntegerPredicate
sle = I.SLE

nsw :: WrapMode
nsw = NSW
nuw :: WrapMode
nuw = NUW
nuw'nsw :: WrapMode
nuw'nsw = NUW'NSW
nsw'nuw :: WrapMode
nsw'nuw = NSW'NUW
