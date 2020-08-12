{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-|

Module that translates each line of LLVM IR into Boolector constraints.

The first portion of the file deals with setup: extracting information from
the user's configuration file, figuring out whether to symex the line, etc.

The second portion of the file implements each instruction that Sys supports.
Each instruction is accompanied by a description of that instruction from
the LLVM reference, and possible comments on how we attempted to follow
that description.

The symex engine uses functions from the Sys DSL (Symex.Symex.Symex).

-}
module Symex.Symex ( symex
                   , SolverResult(..)
                   , Sym.solverSat
                   , Sym.solverUnsat
                   , Sym.solverFailed
                   ) where

import           Checkers.Attack
import           Control.Monad.State.Strict
import           Data.List                  (isInfixOf)
import           Data.Maybe
import           Data.Word                  (Word32)
import           InternalIR.PathInfo
import           InternalIR.SimplePath
import           LLVM.AST.Instruction
import           LLVM.AST.Name              (Name)
import           LLVM.AST.Operand           (Operand)
import qualified LLVM.AST.RMWOperation      as RMW
import           LLVM.AST.Type              hiding (void)
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getConstant)
import           Prelude                    hiding (elem)
import           Symex.CheckerConfigDef
import           Symex.Constant             (Constant, getConstant)
import           Symex.IntegerPredicate
import           Symex.Name
import           Symex.Operand
import qualified Symex.Symex.Boolector      as B
import           Symex.Symex.Symex          (CastKind (..), CastTo (..), Node, SolverResult, Symex)
import qualified Symex.Symex.Symex          as Sym

---
--- Setup
---

-- | Given a user's configuration information (config), a simple path (spath), and
-- statically-generated, information about that path (pathInfo), automatically
-- symbolically execute that path.
symex :: Config a -- ^ User configuration infromation (e.g., refinements)
      -> SimplePath
      -> PathInfo -- ^ Global variables and named types and stuff
      -> IO SolverResult
symex config spath pathInfo = do
  let argsCanAlias = canAlias config
  let strictAlloc = memoryLayout config
      symShadow = symexShadow config
      oob = oobOk config
      to = timeout config
      pathTranslation = forM (instrs spath) $ translateSimpleInstruction config
      refines = symexRefinements config
      initialAction = initializer config
      output = if verbose config then outputResult config else const $ return ()
  ss <- Sym.doSymexAction Sym.blankState $
        ( -- Initialize state and set up the attacker state with the initial action
          Sym.initState strictAlloc argsCanAlias symShadow oob to pathInfo >>
          -- Do the initial action
          initialAction >>
          -- Translate the path
          pathTranslation >>
          -- Solve the translated path constraints
          Sym.solve refines >>
          -- If we're in verbose mode output the result, otherwise just return
          output spath
        )
  return $ Sym.getResult ss

-- | Translate a named instrution
-- Named instructions either have no "name" (or LHS), like "store %x,"
-- or have a name, like "%x = load %p." The name does not come with a type.
translateSimpleInstruction :: Config a -> SimpleInstruction -> Symex a ()
translateSimpleInstruction config simpleInstr = do
  let shouldExecuteFn = preAttack config
  translate <- if isJust shouldExecuteFn
               then shouldExecute (fromJust shouldExecuteFn) simpleInstr
               else return True
  when translate $ case simpleInstr of
    -- Ignore LLVM debug calls
    Instr (Do (Call _ _ _ fname _ _ _)) | isDebug fname -> return ()
    -- Conditionally execute the line if the name on the LHS is used
    Instr instr                         -> executeInstr False instr
    TrackOps ops                        -> forM_ ops trackOperand
    -- Unconditionally execute the line
    PathEq instr                        -> executeInstr True instr
    PathNEq instr                       -> executePathNEq instr
    ParEqs instrs                       -> executeParEqs instrs
    _                                   -> return ()
  forM_ (postAttacks config) $ \a -> postAttack a simpleInstr
  where
    isDebug (Right oper) = "llvm.dbg" `isInfixOf` show oper
    isDebug _            = False
    executeInstr unconditionally instr = do
      case instr of
        name := instr -> do
          usedName <- nameIsUsed name
          when (unconditionally || usedName) $ symexecute instr $ Just name
        Do instr -> symexecute instr Nothing
    symexecute instr maybeName = translateInstruction config maybeName instr

-- | Execute a path-neq instruction. It's like path-eq, but ne and not eq.
executePathNEq :: Named Instruction -> Symex a ()
executePathNEq (name := instruction) = do
  let value = head $ getOperands instruction
  valueSym  <- getOperand value
  resultSym <- getDefinedName name
  neResult  <- B.ne resultSym valueSym
  Sym.assert neResult
executePathNEq _ = error "BUG: unexpected PathNEq"

-- | Execute a par-eqs simple instruction. Each named instruction is a bitcast
-- from a phi node (create with 'phisToPar').  NOTE: We currently don't track
-- dependences of variables and just include everything in a big DNF formula
-- (ORs of ANDs). For example,
--
-- >  x = phi  (y, label one), (z, label two)
-- >  y = phi  (z, label one), (x, label two)
--
-- is translated into a par-eqs:
--
-- >  par-eqs [ [x = y, y_1 = z], [x = z, y = x, y_1 = y] ]
--
-- which this function translates into:
--
-- >  (x = y AND y_1 = z) OR (x = z AND y = x AND y_1 = y)
--
-- Note: this example is delibererately more complex to highlight some of the
-- renaming issues. A simpler example:
--
-- > p1 = phi (x, label y), (w, label z)
-- > p2 = phi (q, label y), (n, label z)
--
-- gets translated to:
--
-- > (p1 = x AND p2 = q) OR (p1 = w AND p2 = n)
--
executeParEqs :: [[Named Instruction]] -> Symex a ()
executeParEqs niss = do
  tt <- B.true
  ff <- B.false
  -- Compute all the ANDs:
  andClauses <- forM niss $ \nis -> do
    nodes <- mapM toEqNode nis
    foldM B.and tt nodes
  -- Compute all the ORs:
  foldM B.or ff andClauses >>= Sym.assert
  where toEqNode (name := instruction) = do let ops = getOperands instruction
                                                value = head ops
                                            valueSym  <- getOperand value
                                            resultSym <- getDefinedName name
                                            B.eq resultSym valueSym
        toEqNode _ = error $ "BUG: ParEq of invalid form: " ++ show niss

---
--- Symbolic execution
---

-- | Translate an LLVM instruction into a series of Boolector constraints
translateInstruction :: Config a -- ^ The configuration options may affect translation
                     -> Maybe Name  -- ^ The lefthand side (e.g., x  in x = y).
                                    -- For 'Do' named instructions, this is nothing.
                     -> Instruction -- ^ The instruction the translate (the rhs)
                     -> Symex a ()
translateInstruction _ maybeName instruction = do

  case instruction of

    _ | (Just binOp) <- getBinaryOperator instruction ->
      translateBinOp name binOp op0 op1
    _ | (Just binOp) <- getOverflowBinaryOperator instruction ->
      translateBinOp name binOp op0 op1
    _ | (Just pred) <- getPredicate instruction ->
      translateCmp name pred op0 op1
    _ | (Just castKind) <- getCastKind instruction ->
      translateCast name castKind op0
    _ | (Just rmwOp) <- getRMWOperator instruction ->
      translateAtomicrmw name rmwOp op0 op1

    InsertElement vector element index _  -> translateInsertElement name vector element index
    ExtractElement vector index _         -> translateExtractElement name vector index
    InsertValue struct element indecies _ -> translateInsertValue name struct element indecies
    ExtractValue struct indecies _        -> translateExtractValue name struct indecies

    Select condition trueBr falseBr _ -> translateSelect name condition trueBr falseBr

    Load _ address _ _ _                         -> translateLoad name address
    Store _ address value _ _ _                  -> translateStore address value
    CmpXchg _ address expected replacement _ _ _ ->
      translateCmpxchg name address expected replacement
    GetElementPtr _ addr indecies _              -> translateGetelementptr name addr indecies
    Alloca ty _ _ _                              -> do
      pointer <- Sym.stackAllocatePointer ty
      -- For now
      Sym.initShadow pointer ty
      Sym.assignVar name pointer

    Call{} | isJust maybeName -> translateCall name
    Call{} -> return ()

    ShuffleVector vector1 vector2 mask _ -> translateShuffleVector name vector1 vector2 mask
    Fence{} -> return ()

    _ -> error $ unwords ["Do not support instruction", show instruction]
  where
    operands = getOperands instruction
    op0 = operands !! 0
    op1 = operands !! 1
    name = fromJust maybeName

-- | Any one of the LLVM binary op instructions (eg add)
--
-- https://llvm.org/docs/LangRef.html#binary-operations
translateBinOp :: Name -- ^ LHS
               -> (Type -> Node -> Node -> Symex a Node) -- ^ Binary op
               -> Operand -- ^ Operand1
               -> Operand -- ^ Operand2
               -> Symex a ()
translateBinOp result binOp operand1 operand2 = do
  opSym1 <- getOperand operand1
  opSym2 <- getOperand operand2
  let ty = typeOf operand1
  binOp ty opSym1 opSym2 >>= Sym.assignVar result

-- | One of the LLVM cast instructions
--
-- https://llvm.org/docs/LangRef.html#conversion-operations
translateCast :: Name -- ^ LHS
              -> CastTo -- ^ Cast
              -> Operand -- ^ Value
              -> Symex a ()
translateCast result cast value = do
  valueSym <- getOperand value
  let oldType = typeOf value
  Sym.castOp cast valueSym oldType >>= Sym.assignVar result

-- | LLVM 'icmp'
--
-- <result> = icmp <cond> <ty> <op1>, <op2>   ; yields i1 or <N x i1>:result
--
-- The ‘icmp’ compares op1 and op2 according to the condition code given as cond.
-- The comparison performed always yields either an i1 or vector of i1 result
translateCmp :: Name -- ^ LHS
             -> (Type -> Node -> Node -> Symex a Node) -- ^ Cmp op
             -> Operand
             -> Operand
             -> Symex a ()
translateCmp result cmpOp op1 op2 = do
  opSym1 <- getOperand op1
  opSym2 <- getOperand op2
  let ty = typeOf op1
  cmpOp ty opSym1 opSym2 >>= Sym.assignVar result

-- | LLVM 'atomicrmw'
--
-- atomicrmw [volatile] <operation> <ty>* <pointer>, <ty> <value>
-- [syncscope("<target-scope>")] <ordering>                   ; yields ty
--
-- The contents of memory at the location specified by the ‘<pointer>’ operand are atomically
-- read, modified, and written back. The original value at the location is returned. The
-- modification is specified by the operation argument
translateAtomicrmw :: Name -- ^ LHS
                   -> (Node -> Node -> Symex a Node) -- ^ RMW op
                   -> Operand -- ^ Address
                   -> Operand -- ^ Value
                   -> Symex a ()
translateAtomicrmw result rmwOp address value = do
  addressSym <- getOperand address
  valueSym <- getOperand value
  let operandType = typeOf value
  -- This is an atomic series of instructions.
  -- The sequence is:
  -- 1. oldValue = load address
  -- 2. newValue = op oldValue value
  -- 3. store newValue address
  -- 4. return oldValue
  oldValueSym <- Sym.load addressSym operandType
  newValueSym <- rmwOp oldValueSym valueSym
  Sym.store addressSym newValueSym operandType
  Sym.assignVar result oldValueSym

-- | LLVM 'extractelement'
--
-- <result> = extractelement <n x <ty>> <val>, <ty2> <idx>  ; yields <ty>
--
-- The result is a scalar of the same type as the element type of val. Its value is the
-- value at position idx of val. If idx exceeds the length of val, the result is a
-- poison value.
translateExtractElement :: Name -- ^ LHS
                        -> Operand -- ^ Vector
                        -> Operand -- ^ Index
                        -> Symex a ()
translateExtractElement name vector index = do
  -- Vector
  vectorSym <- getOperand vector
  let vectorType = typeOf vector
  -- Element
  elementType <- getNameType name
  -- Indecies
  indSym <- getOperand index
  -- Result
  elementSym <- Sym.getElement vectorSym vectorType elementType [indSym]
  Sym.assignVar name elementSym

-- | LLVM 'insertelement'
--
-- <result> = insertelement <n x <ty>> <val>, <ty> <elt>, <ty2> <idx>    ; yields <n x <ty>>
--
-- The result is a vector of the same type as val. Its element values are those of val
-- except at position idx, where it gets the value elt. If idx exceeds the length of val,
-- the result is a poison value.
translateInsertElement :: Name -- ^ LHS
                       -> Operand -- ^ Vector
                       -> Operand -- ^ Element
                       -> Operand -- ^ Index
                       -> Symex a ()
translateInsertElement name vector element index = do
  -- Vector
  vectorSym <- getOperand vector
  let vectorType = typeOf vector
  -- Element
  elementSym <- getOperand element
  -- Indecies
  indexSym <- getOperand index
  -- Result
  resultSym <- Sym.setElement vectorSym vectorType elementSym [indexSym]
  Sym.assignVar name resultSym

-- | LLVM 'shufflevector'
--
-- <result> = shufflevector <n x <ty>> <v1>, <n x <ty>> <v2>, <m x i32> <mask>
-- ; yields <m x <ty>>
--
-- The elements of the two input vectors are numbered from left to right across both of the
-- vectors. The shuffle mask operand specifies, for each element of the result vector,
-- which element of the two input vectors the result element gets. If the shuffle mask is
-- undef, the result vector is undef. If any element of the mask operand is undef, that
-- element of the result is undef. If the shuffle mask selects an undef element from one of
-- the input vectors, the resulting element is undef.
translateShuffleVector :: Name -- ^ LHS
                       -> Operand -- ^ Vector
                       -> Operand -- ^ Other vector
                       -> Constant -- ^ Constant mask
                       -> Symex a ()
translateShuffleVector result vector1 vector2 mask = do
  maskSym <- getConstant mask
  let maskTy = typeOf mask

  vector1Sym <- getOperand vector1
  let vector1Ty = typeOf vector1
  vector2Sym <- getOperand vector2
  let vector2Ty = typeOf vector2

  Sym.shuffleVectorOp maskSym maskTy vector1Sym vector1Ty vector2Sym vector2Ty >>=
     Sym.assignVar result


-- | LLVM 'extractvalue'
--
-- <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}*
--
-- The result is the value at the position in the aggregate specified by the index operands.
translateExtractValue :: Name -- ^ LHS
                      -> Operand -- ^ Struct
                      -> [Word32] -- ^ Indecies
                      -> Symex a ()
translateExtractValue name struct indecies = do
  -- Struct
  structSym <- getOperand struct
  let structType = typeOf struct
  -- Element
  elementType <- getNameType name
  -- Indecies
  indexSyms <- mapM Sym.indexFromWord indecies
  -- Result
  elementSym <- Sym.getElement structSym structType elementType indexSyms
  Sym.assignVar name elementSym

-- | LLVM 'insertvalue'
--
-- <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*
-- ; yields <aggregate type>
--
-- The result is an aggregate of the same type as val. Its value is that of val except
-- that the value at the position specified by the indices is that of elt.
translateInsertValue :: Name -- ^ LHS
                     -> Operand -- ^ Struct
                     -> Operand -- ^ Element
                     -> [Word32] -- ^ Indecies
                     -> Symex a ()
translateInsertValue name struct element indecies = do
  -- Struct
  structSym <- getOperand struct
  let structType = typeOf struct
  -- Element
  elementSym <- getOperand element
  -- Indecies
  indexSyms <- mapM Sym.indexFromWord indecies
  -- Result
  resultSym <- Sym.setElement structSym structType elementSym indexSyms
  Sym.assignVar name resultSym

-- | LLVM 'load'
--
-- <result> = load [volatile] <ty>, <ty>* <pointer>[, align <alignment>][, !nontemporal
-- !<index>][, !invariant.load !<index>][, !invariant.group !<index>][, !nonnull !<index>]
-- [, !dereferenceable !<deref_bytes_node>][, !dereferenceable_or_null
-- !<deref_bytes_node>] [, !align !<align_node>]
--
-- The location of memory pointed to is loaded. If the value being loaded is of scalar type
-- then the number of bytes read does not exceed the minimum number of bytes needed to hold
-- all bits of the type. For example, loading an i24 reads at most three bytes. When loading
-- a value of a type like i20 with a size that is not an integral number of bytes, the result
-- is undefined if the value was not originally written using a store of the same type.
translateLoad :: Name -- ^ LHS
              -> Operand -- ^ Address
              -> Symex a ()
translateLoad value address = do
  valueType <- getNameType value
  addressSym <- getOperand address
  Sym.load addressSym valueType >>= Sym.assignVar value

-- | LLVM 'store'
--
-- store [volatile] <ty> <value>, <ty>* <pointer>[, align <alignment>]
-- [, !nontemporal !<index>][, !invariant.group !<index>]        ; yields void
-- store atomic [volatile] <ty> <value>, <ty>* <pointer> [syncscope("<target-scope>")]
-- <ordering>, align <alignment> [, !invariant.group !<index>] ; yields void
--
-- The contents of memory are updated to contain <value> at the location specified by the
-- <pointer> operand. If <value> is of scalar type then the number of bytes written does not
-- exceed the minimum number of bytes needed to hold all bits of the type. For example,
-- storing an i24 writes at most three bytes. When writing a value of a type like i20 with a
-- size that is not an integral number of bytes, it is unspecified what happens to the extra
-- bits that do not belong to the type, but they will typically be overwritten.
translateStore :: Operand -- ^ Address
               -> Operand -- ^ Value
               -> Symex a ()
translateStore address value = do
  addressSym <- getOperand address
  valueSym <- getOperand value
  Sym.store addressSym valueSym $ typeOf value

-- | LLVM 'getelementptr'
--
-- <result> = getelementptr <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
-- <result> = getelementptr inbounds <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
-- <result> = getelementptr <ty>, <ptr vector> <ptrval>, [inrange] <vector index type> <idx>
--
-- Relevant info on indecies:
--
-- The type of each index argument depends on the type it is indexing into. When indexing
-- into a (optionally packed) structure, only i32 integer constants are allowed (when using
-- a vector of indices they must all be the same i32 integer constant). When indexing into
-- an array, pointer or vector, integers of any width are allowed, and they are not
-- required to be constant. These integers are treated as signed values where relevant.
--
-- https://llvm.org/docs/GetElementPtr.html
translateGetelementptr :: Name
                       -> Operand
                       -> [Operand]
                       -> Symex a ()
translateGetelementptr name addr indecies = do
  addrSym <- getOperand addr
  let addrType = typeOf addr
  when (isVectorType addrType) $ error "Do not support GEP off vector of pointers"
  pointeeTy <- Sym.referentTy addrType
  indexSyms <- mapM getOperand indecies
  resultSym <- getDefinedName name
  void $ Sym.doGep resultSym addrSym pointeeTy indexSyms

-- | If we've hit a call, it's a call we're not entering.
-- Therefore, we may need to allocate space for any pointer returned by the call
-- This space would have otherwise been allocated by the unentered function
translateCall :: Name
              -> Symex a ()
translateCall name = do
  result <- getDefinedName name
  resultType <- getTrackedNameType name
  isPointer <- Sym.isPointerTy resultType
  when isPointer $ Sym.referentTy resultType >>= Sym.heapAllocateMaybeNullPointer result
  Sym.allocPointer result

-- | LLVM 'select'
--
-- <result> = select selty <cond>, <ty> <val1>, <ty> <val2>             ; yields ty
-- selty is either i1 or {<N x i1>}
--
-- If the condition is an i1 and it evaluates to 1, the instruction returns the first value
-- argument; otherwise, it returns the second value argument.
--
-- If the condition is a vector of i1, then the value arguments must be vectors of the same
-- size, and the selection is done element by element.
--
-- If the condition is an i1 and the value arguments are vectors of the same size, then an
-- entire vector is selected.
translateSelect :: Name -- ^ LHS
                -> Operand -- ^ Condition
                -> Operand -- ^ True value
                -> Operand -- ^ False value
                -> Symex a ()
translateSelect result condition trueValue falseValue = do
  conditionSym <- getOperand condition
  trueSym <- getOperand trueValue
  falseSym <- getOperand falseValue
  let conditionType = typeOf condition
      branchType = typeOf trueValue
  Sym.selectOp conditionSym conditionType branchType trueSym falseSym >>=
     Sym.assignVar result

-- | LLVM 'cmpxchg' (only STRONG)
--
-- cmpxchg [weak] [volatile] <ty>* <pointer>, <ty> <cmp>, <ty> <new>
-- [syncscope("<target-scope>")] <success ordering> <failure ordering> ; yields  { ty, i1 }
--
-- The contents of memory at the location specified by the ‘<pointer>’ operand is read and
-- compared to ‘<cmp>’; if the values are equal, ‘<new>’ is written to the location. The
-- original value at the location is returned, together with a flag indicating success
-- (true) or failure (false).
--
-- If the cmpxchg operation is marked as weak then a spurious failure is permitted: the
-- operation may not write <new> even if the comparison matched.
--
-- If the cmpxchg operation is strong (the default), the i1 value is 1 if and only if the
-- value loaded equals cmp.
--
--A successful cmpxchg is a read-modify-write instruction for the purpose of identifying
-- release sequences. A failed cmpxchg is equivalent to an atomic load with an ordering
-- parameter determined the second ordering parameter.
translateCmpxchg :: Name -- ^ LHS
                 -> Operand -- ^ Address
                 -> Operand -- ^ Expected
                 -> Operand -- ^ Replacement
                 -> Symex a ()
translateCmpxchg struct address expected replacement = do
  addressSym <- getOperand address
  expectedSym <- getOperand expected
  replacementSym <- getOperand replacement
  -- Atomic series of instructions is:
  -- 1. oldSym = load address
  -- 2. Compare oldSym and expectedSym
  -- 3. If they are the same, store replacementSym to address
  -- 4. return { oldSym, boolIfTheyWereSame }
  let oldValueType = typeOf expected
  oldValueSym <- Sym.load addressSym oldValueType
  comparison <- Sym.isEq oldValueType oldValueSym expectedSym
  -- The conditional write. We're gonna translate it into a write of
  -- if succes then replacement else oldValue
  let comparisonType = IntegerType 1
      branchType = oldValueType
  conditionalWrite <-
    Sym.selectOp comparison comparisonType branchType replacementSym expectedSym
  Sym.store addressSym conditionalWrite oldValueType
  -- Build the struct
  structType <- getNameType struct
  Sym.aggregate structType [oldValueSym, comparison] >>= Sym.assignVar struct

-- | Given a comparison instruction, return the corresponding boolector function
-- for the comparison predicate (or nothing if the function is not comparison)
getPredicate :: Instruction -- ^ Instruction to translate
             -> Maybe (Type -> Node -> Node -> Symex a Node) -- ^ Comparison function
getPredicate (ICmp predicate _ _ _) = Just $ translatePredicate predicate
getPredicate _                      = Nothing

-- | Given an instruction, return the cast kind if it is cast, and nothing otherwise
getCastKind :: Instruction
            -> Maybe CastTo
getCastKind instr =
  case instr of
    Trunc _ ty _         -> cast ty TruncKind
    ZExt _ ty _          -> cast ty ZExtKind
    SExt _ ty _          -> cast ty SExtKind
    PtrToInt _ ty _      -> cast ty PtrToIntKind
    IntToPtr _ ty _      -> cast ty IntToPtrKind
    BitCast _ ty _       -> cast ty BitCastKind
    AddrSpaceCast _ ty _ -> cast ty UnsupportedKind
    _                    -> Nothing
  where cast ty kind = Just $ CastTo ty kind

-- | The operator for the atomicrmw instruction
--
-- xchg: *ptr = val
-- add: *ptr = *ptr + val
-- sub: *ptr = *ptr - val
-- and: *ptr = *ptr & val
-- nand: *ptr = ~(*ptr & val)
-- or: *ptr = *ptr | val
-- xor: *ptr = *ptr ^ val
-- max: *ptr = *ptr > val ? *ptr : val (using a signed comparison)
-- min: *ptr = *ptr < val ? *ptr : val (using a signed comparison)
-- umax: *ptr = *ptr > val ? *ptr : val (using an unsigned comparison)
-- umin: *ptr = *ptr < val ? *ptr : val (using an unsigned comparison)
getRMWOperator :: Instruction -> Maybe (Node -> Node -> Symex a Node)
getRMWOperator (AtomicRMW _ operation _ value _ _) = do
  let valTy = typeOf value
  Just $ case operation of
    RMW.Xchg -> const return
    RMW.Add  -> Sym.addOp valTy
    RMW.Sub  -> Sym.subOp valTy
    RMW.And  -> Sym.andOp valTy
    RMW.Nand -> Sym.nandOp valTy
    RMW.Or   -> Sym.orOp valTy
    RMW.Xor  -> Sym.xorOp valTy
    RMW.Max  -> conditionalReturn valTy $ Sym.isSgt valTy
    RMW.Min  -> conditionalReturn valTy $ Sym.isSlt valTy
    RMW.UMax -> conditionalReturn valTy $ Sym.isUgt valTy
    RMW.UMin -> conditionalReturn valTy $ Sym.isUlt valTy
getRMWOperator _                                   = Nothing

-- | Helper function for the getRMWOperator function
conditionalReturn :: Type
                  -> (Node -> Node -> Symex a Node) -- ^ The operation
                  -> Node -- ^ The old value
                  -> Node -- ^ The new value
                  -> Symex a Node
conditionalReturn brTy op oldVal newVal = do
  condition <- op oldVal newVal
  Sym.selectOp condition (IntegerType 1) brTy oldVal newVal

getOverflowBinaryOperator :: Instruction -- ^ Instruction to extract LLVM intrinsic from
                          -> Maybe (Type -> Node -> Node -> Symex a Node)
getOverflowBinaryOperator instr =
  case getFunName instr of
    Nothing   -> Nothing
    Just name -> getOp name
  where
    getOp :: Name -> Maybe (Type -> Node -> Node -> Symex a Node)
    getOp name
      | is name "llvm.sadd.with.overflow" = Just Sym.saddOp
      | is name "llvm.uadd.with.overflow" = Just Sym.uaddOp
      | is name "llvm.ssub.with.overflow" = Just Sym.ssubOp
      | is name "llvm.usub.with.overflow" = Just Sym.usubOp
      | is name "llvm.smul.with.overflow" = Just Sym.smulOp
      | is name "llvm.umul.with.overflow" = Just Sym.umulOp
      | otherwise = Nothing
    is x y = y `isInfixOf` show x

-- | Given an instruction, return the binary operator (if its a binary instruction),
-- or nothing (if its not)
getBinaryOperator :: Instruction -- ^ Instruction to extract op from
                  -> Maybe (Type -> Node -> Node -> Symex a Node)
getBinaryOperator instr =
  case instr of
    Add{}  -> Just Sym.addOp
    Sub{}  -> Just Sym.subOp
    Mul{}  -> Just Sym.mulOp
    UDiv{} -> Just Sym.udivOp
    SDiv{} -> Just Sym.sdivOp
    URem{} -> Just Sym.uremOp
    SRem{} -> Just Sym.sremOp
    Shl{}  -> Just Sym.shlOp
    LShr{} -> Just Sym.lshrOp
    AShr{} -> Just Sym.ashrOp
    And{}  -> Just Sym.andOp
    Or{}   -> Just Sym.orOp
    Xor{}  -> Just Sym.xorOp
    _      -> Nothing
