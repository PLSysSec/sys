module LLVMAST.TypeInterface where
import           Data.Word     (Word32, Word64)
import           LLVM.AST.Type

derefType :: Type -> Type
derefType (PointerType referant _) = referant
derefType t                        = error $ "Cannot dereference a non-pointer type: " ++ show t

isIntType :: Type -> Bool
isIntType IntegerType{} = True
isIntType _             = False

isArray :: Type -> Bool
isArray ArrayType{} = True
isArray _           = False

isVector :: Type -> Bool
isVector = isVectorType

isVectorType :: Type -> Bool
isVectorType VectorType{} = True
isVectorType _            = False

getVectorBase :: Type -> Type
getVectorBase (VectorType _ elemType) = elemType
getVectorBase t                       = error $ "Cannot get base type of non-vector: " ++ show t

getArrayBase :: Type -> Type
getArrayBase (ArrayType _ elemType) = elemType
getArrayBase t                      = error $ "Cannot get base type of non-vector: " ++ show t


getVectorNumElems :: Type -> Word32
getVectorNumElems (VectorType num _) = num
getVectorNumElems t                  = error $ "Cannot get num elements of non-vector: " ++ show t

aggrNumElems :: Type -> Word64
aggrNumElems (VectorType num _)      = fromIntegral num
aggrNumElems (StructureType _ elems) = fromIntegral $ length elems
aggrNumElems (ArrayType num _)       = num
aggrNumElems t                       = error $ "Not an aggregate type: " ++ show t

inMemory :: Type -> Maybe MemType
inMemory PointerType{}   = Just Ptr
inMemory VectorType{}    = Just Vect
inMemory StructureType{} = Just Strt
inMemory ArrayType{}     = Just Arr
inMemory _               = Nothing

data MemType = Ptr | Vect | Strt | Arr
             deriving (Eq, Ord, Show)


isPointer :: Type -> Bool
isPointer PointerType{} = True
isPointer _             = False
