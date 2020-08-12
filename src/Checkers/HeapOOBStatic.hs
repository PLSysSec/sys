{-|

This checker looks for cases where there is an allocation followed by
an index operation, and there's a dependency between the size of the allocation
and the size of the index. For example:

x = malloc (y * 4)
z = gep x y

Clearly, most of these instances are not buggy, which is why we need the symbolic
checker (HeapOOBSymbolic.hs)

We HAVE NOT cleaned up this static checker, so its a good example of how messy and
convoluted they can get in the worst case.

-}
module Checkers.HeapOOBStatic where
import           Control.Monad              (when)
import           Control.Monad.State.Strict (liftM)
import           Data.Either                (fromLeft, isLeft)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Set                   as S
import           LLVM.AST
import qualified LLVM.AST.Operand           as O
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getFunName)
import           Prelude                    hiding (rem, round)
import           Static.CheckerState        hiding (addAlias, getAliasKey)

data OOBState = OOBState { deps             :: S.Set (S.Set Name)
                         , mallocSizes      :: M.Map Name (Either Name Int)
                         , structAliases    :: M.Map Name (S.Set Name)
                         -- These are for finding uses
                         , oobStructAliases :: M.Map Name (S.Set Name)
                         , bugInfo          :: M.Map Name OOBInfo
                         }

                deriving (Eq, Ord, Show)

data OOBInfo = OOBInfo { structName'     :: Name
                       , allocationSize' :: Either Name Int
                       , indexSize'      :: Either Name Int
                       , jumpSize'       :: Int
                       , isGep'          :: Bool
                       , oobStructName   :: Name
                       , oobStructInint  :: Int
                       , pathToOOb       :: Path
                       }
             deriving (Eq, Ord, Show)


data MOOBBug = MOOBBug { oobInfo :: OOBInfo
                       , oobFile :: FilePath
                       , oobFun  :: Name
                       , oobPath :: [Name]
                       , oobLine :: Int
                       }
               deriving (Eq, Ord, Show)

blankOOBState :: OOBState
blankOOBState = OOBState S.empty M.empty M.empty M.empty M.empty

moobCheck :: Int
          -> Named Instruction
          -> Checker OOBState MOOBBug ()
moobCheck lineno ninstr =
  case ninstr of

    name := BitCast op _ _ -> addAlias op name >> addOOBAlias op name
    _ := PtrToInt{} -> die
    name := Load _ op _ _ _ -> checkForBug op lineno >> addDep name op
    Do (Store _ op _ _ _ _) -> checkForBug op lineno

    -- For now don't check strlen
    _ := (Call _ _ _ funName _ _ _) | isStrlen funName -> die

    -- Start tracking known sizes
    -- Browser malloc is just @malloc(i64 size)
    -- Freedbsd malloc @malloc(size, type, flags)
    -- Browser realloc is @realloc(ptr, size)
    -- Bsd: realloc(void *addr, unsigned long size, struct malloc_type *type, int flags)
    name := (Call _ _ _ funName argInfos _ _) ->
      when ("alloc" `isInfixOf` show funName && length argInfos >= 1) $ do
        state <- getState
        let msize = fst $ argInfos !! 0
        when (isLocalReference msize) $ do
          let size = getSizeOrSym msize
          putState $ state { mallocSizes = M.insert name size $ mallocSizes state
                           , structAliases = M.insert name (S.fromList [name])
                                             $ structAliases state
                           }

    -- See if those sizes are oob
    (Do (Call _ _ _ funName argInfos _ _)) | isMemCall funName-> do
      let dstBuff = fst $ argInfos !! 0
          srcBuff = fst $ argInfos !! 1
          memOpSize = fst $ argInfos !! 2
      when (isLocalReference memOpSize) $ do
        addOOBMem lineno dstBuff memOpSize 8
        addOOBMem lineno srcBuff memOpSize 8
    name := GetElementPtr _ addr (ind:_) _ -> do
      mJumpSize <- getPointeeSize $ typeOf addr
      when (isJust mJumpSize) $ do
        addOOBGep lineno name addr ind $ round $ fromJust mJumpSize
      addOOBAlias addr name

    name := Add _ _ op1 op2 _ -> addDep name op1 >> addDep name op2
    name := Sub _ _ op1 op2 _ -> addDep name op1 >> addDep name op2
    name := Mul _ _ op1 op2 _ -> addDep name op1 >> addDep name op2
    name := UDiv _ op1 op2 _  -> addDep name op1 >> addDep name op2
    name := SDiv _ op1 op2 _  -> addDep name op1 >> addDep name op2
    name := URem op1 op2 _    -> addDep name op1 >> addDep name op2
    name := SRem op1 op2 _    -> addDep name op1 >> addDep name op2
    name := Shl _ _ op1 op2 _ -> addDep name op1 >> addDep name op2
    name := LShr _ op1 op2 _  -> addDep name op1 >> addDep name op2
    name := AShr _ op1 op2 _  -> addDep name op1 >> addDep name op2
    name := And op1 op2 _     -> addDep name op1 >> addDep name op2
    name := Or op1 op2 _      -> addDep name op1 >> addDep name op2
    name := Xor op1 op2 _     -> addDep name op1 >> addDep name op2
    name := Trunc op _ _      -> addDep name op
    name := ZExt op _ _       -> addDep name op
    name := SExt op _ _       -> addDep name op
    -- Remove dependencies when its compared against some unknown bound.
    _ := ICmp _ op1 op2 _    -> when (isLocalReference op1 && isLocalReference op2) $
      removeDep op1 >> removeDep op2
    _ := Select _ op2 op3 _ -> removeDep op2 >> removeDep op3
    _ -> return ()
  where
    getPointeeSize :: Type -> Checker OOBState MOOBBug (Maybe Int)
    getPointeeSize (PointerType ty _) = getKnownSize ty
    getPointeeSize _                  = return Nothing
    getKnownSize :: Type -> Checker OOBState MOOBBug (Maybe Int)
    getKnownSize (IntegerType it) = return $ Just $ fromIntegral it
    getKnownSize PointerType{}    = return $ Just 32
    getKnownSize (FloatingPointType fp) =
      return $ Just $ case fp of
                 HalfFP      -> 16
                 FloatFP     -> 32
                 DoubleFP    -> 64
                 FP128FP     -> 128
                 X86_FP80FP  -> 80
                 PPC_FP128FP -> 128
    getKnownSize FunctionType{}   = return $ Just 32
    getKnownSize (ArrayType numElems elemType) = do
      size <- getKnownSize elemType
      return $ fmap (* fromIntegral numElems) size
    getKnownSize (VectorType numElems elemType) = do
      size <- getKnownSize elemType
      return $ fmap (* fromIntegral numElems) size
    getKnownSize (NamedTypeReference name) = do
      mty <- getNamedType name
      case mty of
        Just ty -> getKnownSize ty
        Nothing -> return Nothing
    getKnownSize (StructureType _ elems) = do
      sizes <- mapM getKnownSize elems
      if any isNothing sizes
      then return Nothing
      else return $ Just $ sum $ catMaybes sizes
    getKnownSize _                = return Nothing
    round x = case x of
                _ | x < 8 -> error "Should not have value smaller than a byte"
                _ -> let rem = x `mod` 8
                     in if rem == 0 then x else x + rem

removeDep :: Operand -> Checker OOBState MOOBBug ()
removeDep (O.LocalReference _ dep) = do
    moobs <- getState
    let curDeps = deps moobs
        newDeps = S.map (\depSet -> S.delete dep depSet ) curDeps
    putState $ moobs { deps = newDeps }
removeDep _ = return ()

addDep :: Name -> Operand -> Checker OOBState MOOBBug ()
addDep newDep (O.LocalReference _ oldDep) = do
  moobs <- getState
  let curDeps = deps moobs
      newDeps = S.map (\depSet -> if S.member oldDep depSet
                                  then S.insert newDep depSet else depSet
                      ) curDeps
      finalDeps = if curDeps == newDeps
                  then S.insert (S.fromList [newDep, oldDep]) newDeps else newDeps
  putState $ moobs { deps = finalDeps }
addDep _ _ = return ()

isDep :: Name -> Name -> Checker OOBState MOOBBug Bool
isDep name1 name2 = do
  moobs <- getState
  let curDeps = deps moobs
      newDeps = S.filter (\depSet -> S.member name1 depSet && S.member name2 depSet) curDeps
  return $ not $ S.empty == newDeps

getSizeOrSym :: Operand -> Either Name Int
getSizeOrSym (O.LocalReference _ name) = Left name
--getSizeOrSym (O.ConstantOperand (C.Int _ amt)) = Right $ fromIntegral amt
getSizeOrSym _                         = error "Should not be indexing with some other type"

getOOBAliasKey :: Operand -> Checker OOBState MOOBBug (Maybe Name)
getOOBAliasKey (O.LocalReference _ name) = do
  aliases <- oobStructAliases `liftM` getState
  let aliasKeys = M.filter (S.member name) aliases
  return $ case M.keys aliasKeys of
            [key] -> Just key
            []    -> Nothing
            _     -> error "Operand should not have multiple aliases"
getOOBAliasKey _ = return Nothing

getAliasKey :: Operand -> Checker OOBState MOOBBug (Maybe Name)
getAliasKey (O.LocalReference _ name) = do
  aliases <- structAliases `liftM` getState
  let aliasKey = M.filter (S.member name) aliases
  return $ case M.keys aliasKey of
            [key] -> Just key
            []    -> Nothing
            _     -> error "Operand should not have multiple aliases"
getAliasKey _ = return Nothing

addOOBAlias :: Operand -> Name -> Checker OOBState MOOBBug ()
addOOBAlias op aliasName = do
  aliasKey <- getOOBAliasKey op
  when (isJust aliasKey) $ do
    let key = fromJust aliasKey
    state <- getState
    putState $ state { oobStructAliases = M.insertWith S.union key (S.fromList [aliasName]) $
                                          oobStructAliases state }

addAlias :: Operand -> Name -> Checker OOBState MOOBBug ()
addAlias op aliasName = do
  aliasKey <- getAliasKey op
  when (isJust aliasKey) $ do
    let key = fromJust aliasKey
    state <- getState
    putState $ state { structAliases = M.insertWith S.union key (S.fromList [aliasName]) $
                                       structAliases state }

-- Out of bounds

addOOBMem :: Int
          -> Operand
          -> Operand
          -> Int
          -> Checker OOBState MOOBBug ()
addOOBMem lineno op indexOp jumpSize | isLocalReference indexOp = do
  state <- getState
  let sizes = mallocSizes state
  aliasKey <- getAliasKey op
  when (isJust aliasKey) $ do
    let addrName = fromJust aliasKey
    when (M.member addrName sizes) $ do
      path <- getPath
      let allocSize = sizes M.! addrName
          indexSize = getSizeOrSym indexOp
          allocVar' = fromLeft (mkName "nope") allocSize
          indexVar = fromLeft (mkName "nope") indexSize
          oobInfo' = OOBInfo addrName allocSize indexSize
                    jumpSize False addrName lineno path
      if isLeft allocSize && isLeft indexSize
      then do
        areDeps <- isDep allocVar' indexVar
        if areDeps
        then do
          fp <- getFilepath
          fun <- getFunName
          addBug $ MOOBBug oobInfo' fp fun path lineno
        else return ()
      else do
        fp <- getFilepath
        fun <- getFunName
        addBug $ MOOBBug oobInfo' fp fun path lineno
addOOBMem _ _ _ _ = return ()

addOOBGep :: Int
          -> Name
          -> Operand
          -> Operand
          -> Int
          -> Checker OOBState MOOBBug ()
addOOBGep lineno newAddr op indexOp jumpSize | isLocalReference indexOp = do
  state <- getState
  let sizes = mallocSizes state
  aliasKey <- getAliasKey op
  when (isJust aliasKey) $ do
    let addrName = fromJust aliasKey
    when (M.member addrName sizes) $ do
      path <- getPath
      let allocSize = sizes M.! addrName
          indexSize = getSizeOrSym indexOp
          allocVar' = fromLeft (mkName "nope") allocSize
          indexVar = fromLeft (mkName "nope") indexSize
          oobInfo' = OOBInfo addrName allocSize indexSize
                    jumpSize True newAddr lineno path
      if isLeft allocSize && isLeft indexSize
      then do
        areDeps <- isDep allocVar' indexVar
        if areDeps && (allocSize /= indexSize)
        then do
          putState $ state { oobStructAliases = M.insert newAddr (S.fromList [newAddr])
                                                $ oobStructAliases state
                           , bugInfo = M.insert newAddr oobInfo' $ bugInfo state
                           }
        else return ()
      else do
        -- Start tracking this new name to see if its ever loaded or stored
        -- Save the oob information in case we fnd a bug later on
        putState $ state { oobStructAliases = M.insert newAddr (S.fromList [newAddr])
                                              $ oobStructAliases state
                         , bugInfo = M.insert newAddr oobInfo' $ bugInfo state
                         }
addOOBGep _ _ _ _ _ = return ()

checkForBug :: Operand
            -> Int
            -> Checker OOBState MOOBBug ()
checkForBug op lineno = do
  aliasKey <- getOOBAliasKey op
  case aliasKey of
    Nothing -> return ()
    Just name -> do
      bugInfos <- bugInfo `liftM` getState
      let oobInfo' = bugInfos M.! name
      fp <- getFilepath
      fun <- getFunName
      path <- getPath
      addBug $ MOOBBug oobInfo' fp fun path lineno

