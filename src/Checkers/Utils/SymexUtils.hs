module Checkers.Utils.SymexUtils where
import           Data.List             (isInfixOf)
import           InternalIR.SimplePath
import           LLVM.AST.Instruction  hiding (args)
import           LLVM.AST.Type
import           LLVM.AST.Typed
import           LLVMAST.Interface
import           Prelude               hiding (repeat)
import           Symex.Operand
import           Symex.Symex.Symex

doMemset :: SimpleInstruction
         -> Symex a Bool
doMemset instr = do
  case getNamedInstr instr of
    Just (Do (Call _ _ _ fnname args _ _)) -> do
      if (("memset" `isInfixOf` show fnname) && (length args >= 3)
         && typeOf (fst $ args !! 1) == IntegerType 8)
      then do
        let len = fst $ args !! 2
        if isConstantOperand len
        then do
          dstSym <- getOperand $ fst $ args !! 0
          valSym <- getOperand $ fst $ args !! 1
          let repeats = getOperandIntConstant len
          toWrite <- repeat valSym $ fromIntegral repeats
          storeBits dstSym toWrite $ repeats * 8
          return True
        else do
          return False
      else return False
    _                                      -> return False

-- | Not perfect, doesnt slam overlapping pointers. Equivalent to memmove
doMemcpy :: SimpleInstruction
         -> Symex a Bool
doMemcpy instr = memmoveOperation instr "memcpy"

doMemmove :: SimpleInstruction
          -> Symex a Bool
doMemmove instr = memmoveOperation instr "memmove"

memmoveOperation :: SimpleInstruction
                 -> String
                 -> Symex a Bool
memmoveOperation instr callName = do
  case getNamedInstr instr of
    Just (Do (Call _ _ _ fnname args _ _)) -> do
      if ((callName `isInfixOf` show fnname) && (length args >= 3))
      then do
        -- Constant size memcpy
        let len = fst $ args !! 2
        if isConstantOperand len
        then do
          dstSym <- getOperand $ fst $ args !! 0
          srcSym <- getOperand $ fst $ args !! 1
          let readSize = fromIntegral $ 8 * getOperandIntConstant len
          toWrite <- loadBits srcSym $ fromIntegral readSize
          storeBits dstSym toWrite readSize
          return True
        else do
          return False
      else return False
    _                                      -> return False

doMalloc :: SimpleInstruction
         -> Symex a Bool
doMalloc = undefined

doCalloc :: SimpleInstruction
         -> Symex a Bool
doCalloc = undefined
