{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |

This module defines 'ModuleInfo', a data type used to wrap and cache LLVM's
'Module's, but further expose useful information such as types. data types

-}
module InternalIR.ModuleInfo (
    ModuleInfo(..)
  , ProgramInfo
  , modName
  , getModuleInfo
  , getModuleInfoFromFile
  , getFunctionInAnyModule
) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.ByteString.Short   (fromShort)
import           Data.ByteString.UTF8    (toString)
import           Data.Hashable
import qualified Data.Map                as M
import           Data.Maybe              (mapMaybe)
import           LLVM.AST
import qualified LLVM.AST.Constant       as C
import           LLVM.Context
import           LLVM.Module             hiding (Module)
import           LLVMAST.ASTInterface
import           LLVMAST.OrdInstances    ()
import           LLVMAST.RenameVariables (renameVariables)
import           System.FilePath         (takeFileName)

-- | An LLVM Module, along with Information which needs to be computed only once for each module.
-- Some of this info is used to quit early when doing path search.
data ModuleInfo = ModuleInfo { modAST              :: Module  -- ^ LLVM Module itself
                             , modProgramTypes     :: M.Map Name Type
                             , modProgramConstants :: M.Map Name C.Constant
                             , modProgramAliases   :: M.Map Name C.Constant
                             } deriving (Eq, Ord)

-- | Get module name.
modName :: ModuleInfo -> String
modName = takeFileName . toString . fromShort . moduleSourceFileName . modAST

instance Show ModuleInfo where
  show mi = "<ModuleInfo for module " ++ modName mi ++ ">"

instance Hashable Name where
  hashWithSalt i (Name n)   = hashWithSalt i n
  hashWithSalt i (UnName n) = hashWithSalt i n

-- | Acquire an LLVM Module, rename its variables properly, and precompute ModuleInfo
getModuleInfo :: Context -- ^ LLVM Context object
              -> ByteString -- ^ File contents (from Data.ByteString.readFile)
              -> IO ModuleInfo
getModuleInfo ctx contents =
  withModuleFromLLVMAssembly ctx contents $ \m -> do
    ast <- renameVariables <$> moduleAST m
    return $ moduleInfoFromModule ast

-- | Like `getModuleInfo`, but also parses in the file. Takes a filepath.
getModuleInfoFromFile :: Context -> FilePath -> IO ModuleInfo
getModuleInfoFromFile ctx path = do
  contents <- B.readFile path
  getModuleInfo ctx contents

-- | Precompute module info from an LLVM Module.
moduleInfoFromModule :: Module -> ModuleInfo
moduleInfoFromModule modast = let defns = moduleDefinitions modast
                              in  ModuleInfo { modAST              = modast
                                             , modProgramTypes     = getProgramTypes defns
                                             , modProgramConstants = getProgramConstants defns
                                             , modProgramAliases   = getProgramAliases defns
                                             }

getProgramTypes :: [Definition] -> M.Map Name Type
getProgramTypes defns =
  let types = map (\def -> case def of
                             TypeDefinition name (Just ty) -> [(name, ty)]
                             _                             -> []) defns
  in M.fromList $ concat types

getProgramConstants :: [Definition] -> M.Map Name C.Constant
getProgramConstants defns =
  let f def = case def of
        GlobalDefinition glob -> case glob of
          GlobalVariable name _ _ _ _ _ _ _ _ (Just constant) _ _ _ _ ->
              [(name, constant)]
          _ -> []
        _ -> []
  in M.fromList $ concatMap f defns

getProgramAliases :: [Definition] -> M.Map Name C.Constant
getProgramAliases defns =
  let f def = case def of
        GlobalDefinition glob -> case glob of
          GlobalAlias name _ _ _ _ _ _ _ constant -> [(name, constant)]
          _                                       -> []
        _ -> []
  in M.fromList $ concatMap f defns


-- | For now, this is just a list of 'ModuleInfo's for all modules we're interested in.
-- Later, we may want this to be a more coherent (easier-to-search) data
-- structure; for instance, a HashMap Name (ModuleInfo, Callsite) recording all
-- callsites of a given function across all modules.
type ProgramInfo = [ModuleInfo]

-- | Find a function across all modules, and return both the function (as a
-- Global) and the module it was found in. Or, return Nothing if the function
-- could not be found in any module.
--
-- Only returns the function is it was actually *defined*, i.e., not just a forward
-- declaration. See notes on definedFunctionFromNameAndModule in LLVMAST.ASTInterface.hs.
getFunctionInAnyModule :: ProgramInfo -> Name -> Maybe (ModuleInfo, Global)
getFunctionInAnyModule pInfo funcName =
  case mapMaybe (getFunctionInModule funcName) pInfo of
    x:_ -> Just x  -- return the first one found, regardless of whether there are more or not. Laziness should make this a quit-early search.
    []  -> Nothing
  where getFunctionInModule :: Name -> ModuleInfo -> Maybe (ModuleInfo, Global)
        getFunctionInModule fName mInfo = fmap (\x -> (mInfo, x)) $ definedFunctionFromNameAndModule (modAST mInfo) fName
