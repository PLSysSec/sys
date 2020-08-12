{- |

This module defines coarse-grained 'StaticPath's (produced by the static
analysis pass) and a way to expand these paths to fine-grained 'SimplePath's
(used for symbolic execution).

-}
module InternalIR.PathExpand (
  -- * Static paths
    StaticPath, StaticPathEntry(..), SPEIndex(..)
  -- ** Expanding static paths to simple paths
  , staticToSimple
  -- *** Helpers
  , namedInstrFromEntry', namedInstrFromEntry
  ) where

import           Data.Maybe            (fromJust, isNothing, maybeToList)
import           InternalIR.SimplePath
import           LLVM.AST
import qualified LLVM.AST.Constant     as C
import           LLVM.AST.Typed
import           LLVMAST.ASTInterface

-- | A static path entry is a (module, function, basic block, offset into basic
-- block) tuple
data StaticPathEntry = StaticPathEntry {
    speModule         :: ModuleInfo
  , speFunction       :: Name
  , speBasicBlockName :: Name
  , speIndex          :: SPEIndex
  , spePreviousBlock  :: Maybe Name
  }
  deriving (Eq)

-- | Get the actual basic block corresponding to the path entry
speBasicBlock :: StaticPathEntry -> BasicBlock
speBasicBlock ent =
  let bbs0 = fromJust $ bbsFromFunctionName (modAST $ speModule ent) (speFunction ent)
  in head $ filter (\bb -> speBasicBlockName ent == getBlockName bb) bbs0

instance Show StaticPathEntry where
  show spe = unwords [ "<", modName $ speModule spe
                     , ",", show $ speFunction spe
                     , ",", show $ speBasicBlockName spe
                     , ",", show $ speIndex spe
                     , ">" ]

-- | Index into the basic block
data SPEIndex = SPEInstr { speInstrIndex :: Int }
              -- ^ Instruction into basic block to transform
              | SPECall { speCallIndex :: Int, willRet :: Bool }
              -- ^ Call to enter in the basic block
              | SPETerm { speNextBlock :: Name }
              -- ^ Basic block terminator next
              deriving (Show, Eq)

-- | A static path is a list of path entries
type StaticPath = [StaticPathEntry]

-- | Convert a static path to a simple path
staticToSimple :: StaticPath -> SimplePath
staticToSimple [] = error "staticToSimple: called with empty path"
staticToSimple sp0 = let (sip0, sp1) = translatePhiNodesIfFirst sp0
                         SimplePath is1 bbs1 = doStaticToSimple sip0 [] sp1
                     in SimplePath (reverse is1) (reverse bbs1)

-- | We may start at a basic block where we have phis, turn the phis into
-- parallel instructions and rest of basic block.
translatePhiNodesIfFirst :: StaticPath -> (SimplePath, StaticPath)
translatePhiNodesIfFirst sp@(ent:_) | isDanglingPhiNode ent =
    let (phis, rest) = span isDanglingPhiNode sp
        si = phisToPar $ map namedInstrFromEntry' phis
        -- TODO: we can sanity check to make sure that all the phis are actually from same block
        bbEnt = SimplePathBBEntry { spbeMod  = speModule ent
                                  , spbeFunc = speFunction ent
                                  , spbeBB   = speBasicBlock ent }
    in (SimplePath [si] [bbEnt], rest)
translatePhiNodesIfFirst sp = (emptySimplePath, sp)

-- | Is this is a phi node with no previous block?
isDanglingPhiNode :: StaticPathEntry -> Bool
isDanglingPhiNode ent = case speIndex ent of
                          SPEInstr _ -> isNothing (spePreviousBlock ent) && isPhi (namedInstrFromEntry' ent)
                          _ -> False

-- | Get named instruction corresponding to the entry. NOTE: expected type to be an SPEIndex
namedInstrFromEntry' :: StaticPathEntry -> Named Instruction
namedInstrFromEntry' spe = case namedInstrFromEntry spe of
  Just ni -> ni
  _       -> error "namedInstrFromEntry': BUG: called on invalid type"

-- | Get named instruction corresponding to the entry.
namedInstrFromEntry :: StaticPathEntry -> Maybe (Named Instruction)
namedInstrFromEntry ent | isSPEInstr ent = Just $ getBlockContents (speBasicBlock ent) !! (speInstrIndex . speIndex $ ent)
  where isSPEInstr ent' = case speIndex ent' of
                            SPEInstr _ -> True
                            _          -> False
namedInstrFromEntry _ = Nothing

-- | Actual worker function
doStaticToSimple :: SimplePath -- ^ Current simple path
                 -> [Name]     -- ^ Stack of call return variables
                 -> StaticPath -- ^ The rest of the simple path to handle
                 -> SimplePath -- ^ Final path
doStaticToSimple sp0 _  []         = sp0 -- We're done
doStaticToSimple sp0 rets0 (ent:rest) =
  let (rets1, si) = case speIndex ent of
                SPEInstr _  -> (rets0, [translateInstr ent])
                SPETerm _   -> translateTerm ent rets0
                SPECall _ _ -> translateCall ent (head rest)
  in doStaticToSimple (SimplePath si [bbEnt]  <> sp0) rets1 rest
  where bbEnt = SimplePathBBEntry { spbeMod  = speModule ent
                                  , spbeFunc = speFunction ent
                                  , spbeBB   = speBasicBlock ent }

-- | Translate an instruction path entry into a simple instruction.
translateInstr :: StaticPathEntry -> SimpleInstruction
translateInstr ent = let ni = namedInstrFromEntry' ent
                     in case ni of
                          (nv := Phi ty ivs meta) -> setEq nv ty (fst . head $ filter ((== prevBlock) . snd) ivs) meta
                          _ -> Instr ni
  where prevBlock = case spePreviousBlock ent of
                      Just pb -> pb
                      _ -> error "translateInstr: handling phi instruction, expected previous block"

-- | Translate a terminator.
translateTerm :: StaticPathEntry -> [Name] -> ([Name], [SimpleInstruction])
translateTerm ent rets0 = case getBlockTerminator $ speBasicBlock ent of
   Do (CondBr op trueBr _ meta) -> (rets0, [mkPathCond op (mkBool $ nextBlock == trueBr) meta])
   Do (Switch op _ otherDsts meta) ->
     case map fst $ filter ((==nextBlock) . snd) otherDsts of
        []  -> (rets0, map (\(c, _) -> mkNotPathCond op c meta) otherDsts)
        [c] -> (rets0, [mkPathCond op c meta]) -- one of the destinations
        cs  -> (rets0, [switchToPar op cs meta]) -- same destiation multiple values
   Do rtt@(Ret (Just oper) meta) -> let ty = typeOf oper
                                in case rets0 of
                                    (nvar:rets1) -> (rets1, [setEq nvar ty oper meta])
                                    [] -> error $ "BUG: ret expected to have at least one return value: " ++ show ent ++ " WTF: " ++ show rtt
   Do (Br _ _)        -> ([], [])
   Do (Ret Nothing _) -> ([], [])
   term -> error $ "translateTerm: not yet supported: " ++ show term
  where mkPathCond op c meta = case op of
          LocalReference ty nvar -> setPathEq nvar ty (ConstantOperand c) meta
          _                      -> error $ "mkPathCond: not yet supported: " ++ show op
        mkNotPathCond op c meta = case op of
          LocalReference ty nvar -> setPathNEq nvar ty (ConstantOperand c) meta
          _                      -> error $ "mkNotPathCond: not yet supported: " ++ show op
        mkBool tf = C.Int 1 (if tf then 1 else 0)
        nextBlock =  speNextBlock $ speIndex $ ent

-- | Translate a call instruction, i.e., handle call argument stiching.
translateCall :: StaticPathEntry -> StaticPathEntry -> ([Name], [SimpleInstruction])
translateCall ent callEnt =
 let (params, varArgs) = fromJust $ paramsFromFunctionName (modAST $ speModule callEnt) (speFunction callEnt)
     eqIns = zipWith setEqPO params actualFuncArgs
 in if varArgs
      then error "translateCall: varargs not yet supported"
      else (if not (willRet $ speIndex ent)
              then []
              else maybeToList mRetVar, eqIns)
  where namedInstr = getBlockContents (speBasicBlock ent) !! (speCallIndex . speIndex $ ent)
        callInstr  = withoutName namedInstr
        mRetVar    = getResultName namedInstr
        actualFuncArgs = map fst $ arguments callInstr
        setEqPO (Parameter ty n _) op = setEq n ty op []

-- | Is the instruction a phi
isPhi :: Named Instruction -> Bool
isPhi (_ := Phi {}) = True
isPhi _             = False
