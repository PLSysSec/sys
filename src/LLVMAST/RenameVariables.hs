{- |

This module exports a function ('renameVariables') that is used to rename all
function variables to be prefixed by the function name.

-}

module LLVMAST.RenameVariables ( renameVariables
                               -- * Rename monad
                               , RewriteName(..)
                               , NameMap
                               , RenameMonad
                               , getRewriteName
                               , putRewriteName
                               -- ** Rename helpers
                               , rewriteNamedInstruction 
                               , rewriteInstruction
                               , rewriteTerminator
                               , rewriteOperand 
                               , rewriteName 
                               , rewriteNameToName 
                               -- ** Helpers
                               , ppn
                               ) where
import           LLVM.AST
import           LLVM.AST.Global
import           LLVM.Pretty                (ppll)

import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text.Lazy             (unpack)

import           Control.Monad.State.Strict

-- | Rename a variable. Can provide a prefix and version. We rename in
-- 'rewriteNameToName' using the optional prefix and version number.
data RewriteName = RewriteName { rnPrefix :: Maybe Name
                               , rnName   :: Name
                               , rnVer    :: Maybe Int }
                   deriving (Eq, Ord, Show)

-- | Variable rename map.
type NameMap = Map Name RewriteName

-- | We use a state monad to keep track of the names to rewrite in 2nd pass
type RenameMonad = State NameMap

-- | Get rewrite name, if any.
getRewriteName :: Name -> RenameMonad (Maybe RewriteName)
getRewriteName k = do
  nMap <- get
  return $ Map.lookup k nMap

-- | Update existing name, if any.
putRewriteName :: Name -> RewriteName -> RenameMonad ()
putRewriteName k v = do
  nMap <- get
  put $ Map.insert k v nMap

-- | Rewrite all function basic blocks to prefix the name with function name.
renameVariables :: Module -> Module
renameVariables m = m { moduleDefinitions = ds }
  where ds = map rewriteDefinition $ moduleDefinitions m

rewriteDefinition :: Definition -> Definition
rewriteDefinition dfn = case dfn of
  GlobalDefinition global -> GlobalDefinition $ rewriteGlobal global
  x                       -> x

rewriteGlobal :: Global -> Global
rewriteGlobal f@Function{ name = n
                         , parameters = params@(ps, isVar), basicBlocks = bbs} =
  f { basicBlocks = rewriteBasicBlocks n (fst params) bbs
    , parameters  = (rewriteParameters n ps, isVar) }
rewriteGlobal x = x

rewriteParameters :: Name -> [Parameter] -> [Parameter]
rewriteParameters fName params = map f params 
  where f (Parameter ty vName attrs) =
            Parameter ty (prefixWithFunctionName fName vName) attrs

-- | Rewrites all the names in basic blocks by prefixing the with the function name.
rewriteBasicBlocks :: Name -> [Parameter] -> [BasicBlock] -> [BasicBlock]
rewriteBasicBlocks fName params bbs =
  let names = paramNames ++ concatMap extractNames bbs
      nMap  = Map.fromList $ map mk names
  in evalState (mapM rewriteBasicBlock bbs) nMap
    where mk old = (old, RewriteName (Just fName) old Nothing)
          paramNames = map (\(Parameter _ n _) -> n) params

-- | Print name
ppn :: Name -> String
ppn = unpack . ppll

rewriteBasicBlock :: BasicBlock -> RenameMonad BasicBlock
rewriteBasicBlock (BasicBlock n nis0 nt0) = do
  nis1 <- mapM rewriteNamedInstruction nis0
  nt1  <- rewriteNamedTerminator nt0
  return $ BasicBlock n nis1 nt1

rewriteNamedTerminator :: Named Terminator -> RenameMonad (Named Terminator)
rewriteNamedTerminator (n0 := t0) = do
  t1 <- rewriteTerminator t0
  n1 <- rewriteName n0
  return $ n1 := t1
rewriteNamedTerminator (Do t0) = do
  t1 <- rewriteTerminator t0
  return $ Do t1

rewriteNamedInstruction :: Named Instruction -> RenameMonad (Named Instruction)
rewriteNamedInstruction (n0 := i0) = do
  i1 <- rewriteInstruction i0
  n1 <- rewriteName n0
  return $ n1 := i1
rewriteNamedInstruction (Do i0) = do
  i1 <- rewriteInstruction i0
  return $ Do i1

rewriteName :: Name -> RenameMonad Name
rewriteName n0 = do
  mn1 <- getRewriteName n0
  return $ maybe n0 rewriteNameToName mn1

rewriteTerminator :: Terminator -> RenameMonad Terminator
rewriteTerminator term = case term of
  t@(Ret (Just o0) _)      -> (\o1 -> t { returnOperand = Just o1 }) <$>
                              rewriteOperand o0
  t@(CondBr o0 _ _ _)      -> (\o1 -> t { condition = o1 }) <$> rewriteOperand o0
  t@(Switch o0 _ _ _)      -> (\o1 -> t { operand0' = o1 }) <$> rewriteOperand o0
  t@(IndirectBr o0 _ _)    -> (\o1 -> t { operand0' = o1 }) <$> rewriteOperand o0
  t@(Invoke { arguments' = args0 }) -> do
    args1 <- forM args0 $ \(o0, attrs) -> do o1 <- rewriteOperand o0
                                             return (o1, attrs)
    return $ t { arguments' = args1 }
  t@(Resume o0 _)          -> (\o1 -> t { operand0' = o1 }) <$> rewriteOperand o0
  t@(CleanupRet o0 _ _)    -> (\o1 -> t { cleanupPad = o1 }) <$> rewriteOperand o0
  t@(CatchRet o0 _ _)      -> (\o1 -> t { catchPad = o1 }) <$> rewriteOperand o0
  t@(CatchSwitch o0 _ _ _) -> (\o1 -> t { parentPad' = o1 }) <$> rewriteOperand o0
  t -> return t

rewriteInstruction :: Instruction -> RenameMonad Instruction
rewriteInstruction instr = case instr of
  i@(Add _ _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(FAdd _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Sub _ _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(FSub _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Mul _ _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(FMul _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(UDiv _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(SDiv _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(FDiv _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(URem o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(SRem o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(FRem _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Shl _ _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(LShr _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(AShr _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(And o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Or o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Xor o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Alloca _ (Just o0) _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { numElements = Just o0' }
  i@(Load _ o0 _ _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { address = o0' }
  i@(Store _ o0 o1 _ _ _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { address = o0', value = o1' }
  i@(GetElementPtr _ addr0 idxs0 _) -> do
    addr1 <- rewriteOperand addr0
    idxs1 <- mapM rewriteOperand idxs0
    return $ i { address = addr1, indices = idxs1 }
  i@(CmpXchg _ addr o0 o1 _ _ _) -> do
    addr' <- rewriteOperand addr
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { address = addr', expected = o0', replacement = o1' }
  i@(AtomicRMW _ _ addr0 value0 _ _) -> do
    addr1 <- rewriteOperand addr0
    value1 <- rewriteOperand value0
    return $ i { address = addr1, value = value1 }
  i@(Trunc o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(ZExt o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(SExt o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(FPToUI o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(FPToSI o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(UIToFP o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(SIToFP o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(FPTrunc o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(FPExt o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(PtrToInt o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(IntToPtr o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(BitCast o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(AddrSpaceCast o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { operand0 = o0' }
  i@(ICmp _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(FCmp _ o0 o1 _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(Phi { incomingValues = ivs }) -> do
    ivs' <- forM ivs $ \(o, n) -> do o' <- rewriteOperand o
                                     return (o', n)
    return $ i { incomingValues = ivs' }
  i@(Call{ arguments = args0 }) -> do
    args1 <- forM args0 $ \(o0, attrs) -> do o1 <- rewriteOperand o0
                                             return (o1, attrs)
    return $ i { arguments = args1 }
  i@(Select { condition' = o0, trueValue = o1, falseValue = o2 }) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    o2' <- rewriteOperand o2
    return $ i { condition' = o0', trueValue = o1', falseValue = o2' }
  i@(VAArg o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { argList = o0' }
  i@(ExtractElement { vector = o0, index = o1 }) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { vector = o0', index = o1' }
  i@(InsertElement { vector = o0, element = o1, index =o2 }) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    o2' <- rewriteOperand o2
    return $ i { vector = o0', element = o1', index = o2' }
  i@(ShuffleVector o0 o1 _ _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { operand0 = o0', operand1 = o1' }
  i@(ExtractValue o0 _ _) -> do
    o0' <- rewriteOperand o0
    return $ i { aggregate = o0' }
  i@(InsertValue o0 o1 _ _) -> do
    o0' <- rewriteOperand o0
    o1' <- rewriteOperand o1
    return $ i { aggregate = o0', element = o1' }
  i@(CatchPad { catchSwitch = o0, args = args0 }) ->do
    o0' <- rewriteOperand o0
    args0' <- mapM rewriteOperand args0
    return $ i { catchSwitch = o0', args = args0' }
  i@(CleanupPad { parentPad = o0, args = args0 }) ->do
    o0' <- rewriteOperand o0
    args0' <- mapM rewriteOperand args0
    return $ i { parentPad = o0', args = args0' }
  i -> return i

rewriteOperand :: Operand -> RenameMonad Operand
rewriteOperand (LocalReference ty n0) = LocalReference ty <$> rewriteName n0
rewriteOperand (MetadataOperand md)   = MetadataOperand <$> rewriteMetadata md
rewriteOperand op                     = return op

rewriteMetadata :: Metadata -> RenameMonad Metadata
rewriteMetadata (MDValue op) = MDValue <$> rewriteOperand op
rewriteMetadata md           = return md

-- | Getall the names used as lhs
extractNames :: BasicBlock -> [Name]
extractNames (BasicBlock _ nis nt) = case nt of
                                      (n := _) -> ns ++ [n]
                                      _        -> ns
  where ns = map (\(n := _) -> n) $ filter isNamed nis

-- | Is this actually named?
isNamed :: Named a -> Bool
isNamed (_ := _) = True
isNamed _        = False

-- | Prefix function variable name with function name.
prefixWithFunctionName :: Name -> Name -> Name
prefixWithFunctionName fName vName = 
  mkName $ ppn fName ++ "_" ++ ppn vName

-- | Convert a rewrite name to a normal name.
rewriteNameToName :: RewriteName -> Name
rewriteNameToName (RewriteName mPrefix vName mVer) = case (mPrefix, mVer) of
  (Just pfix, Just ver) -> mkName $ ppn pfix ++ "_" ++ ppn vName ++ "_" ++ show ver
  (Just pfix, _)        -> mkName $ ppn pfix ++ "_" ++ ppn vName
  (_, Just ver)         -> mkName $ ppn vName ++ "_" ++ show ver
  _                     ->  vName
