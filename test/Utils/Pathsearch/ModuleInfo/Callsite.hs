{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   Definte the notion of a call site. This is largely from the previous
   implementation and will likely go away.
-}
module Utils.Pathsearch.ModuleInfo.Callsite (
   Callsite(..)
 , modCallsites 
 , getAllCallsites 
 ) where
 
import qualified Data.HashMap.Lazy       as HM
import           Data.Maybe              (catMaybes)
import           LLVM.AST
import           LLVMAST.ASTInterface
import           LLVMAST.OrdInstances    ()
import           InternalIR.ModuleInfo

-- | Get the Map of function name to a list of callsites for that function (within the module)
modCallsites :: ModuleInfo -> HM.HashMap Name [Callsite]
modCallsites = getAllCallsites' . moduleDefinitions . modAST
 
-- | A call site is the call instruction in a function's BB at a particular offset.
data Callsite = Callsite { csFunc :: Global
                         -- ^ We assert this Global is always a Function. This
                         -- is the Function containing the call instruction.
                         , csBB   :: BasicBlock
                         -- ^ The basic block in the Function above, which
                         -- contains the call instruction
                         , csI    :: Int
                         -- ^ Index of the call instruction in the instruction
                         -- list of the BasicBlock. Basic block terminators
                         -- cannot be callsites, only normal instructions can.
                         } deriving (Eq, Show)

instance Ord Callsite where
  compare cs1 cs2 = compare (getFunctionName' $ csFunc cs1, csBB cs1, csI cs1)
                            (getFunctionName' $ csFunc cs2, csBB cs2, csI cs2)
 
getAllCallsites' :: [Definition] -> HM.HashMap Name [Callsite]
getAllCallsites' defns = HM.fromListWith (++) $ concatMap getCallsitesInDefn defns
  where getCallsitesInDefn :: Definition -> [(Name, [Callsite])]
        getCallsitesInDefn defn | isFunction defn =
          concatMap (getCallsitesInBB defn) $ getFunctionBBs' defn
        getCallsitesInDefn _ = []
        getCallsitesInBB defn bb = catMaybes $ zipWith (callsite defn bb) (getBlockContents bb) [0..]
        callsite :: Definition -> BasicBlock -> Named Instruction -> Int -> Maybe (Name, [Callsite])
        callsite defn bb ni idx | i@(Call {}) <- withoutName ni = case nameOfCalledFunc i of
          Just name -> Just (name, [Callsite { csFunc = functionFromDefinition' defn
                                             , csBB   = bb
                                             , csI    = idx
                                             }])
          Nothing   -> Nothing
        callsite _    _  _  _   = Nothing

-- | Get all the callsites of a given function, across all modules
getAllCallsites :: ProgramInfo -> Name -> [(ModuleInfo, Callsite)]
getAllCallsites pInfo funcName = concatMap (getCallsites funcName) pInfo
  where getCallsites :: Name -> ModuleInfo -> [(ModuleInfo, Callsite)]
        getCallsites fName mInfo = map (\x -> (mInfo, x)) $ HM.lookupDefault [] fName $ modCallsites mInfo
