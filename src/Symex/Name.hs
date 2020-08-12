-- | Module for translating LLVM names into their Boolector variable counterparts.  
module Symex.Name ( getDefinedName
                  , nameIsTracked
                  , nameIsUsed
                  , getTrackedNameType
                  , getNameType
                  , makeVarName
                  )
where
import           Control.Monad       (void)
import           LLVM.AST.Name
import           LLVM.AST.Type       hiding (void)
import           InternalIR.PathInfo (makeVarName)
import           Symex.Symex.Symex   as Sym
import           Symex.Symex.Symex   (Node, Symex)

-- | Get a variable assigned to in an IR operation
getDefinedName :: Name -> Symex a Node
getDefinedName llvmName = getVariableFromProgram $ makeVarName llvmName

-- | Is the symbolic execution backend already tracking the variable?
nameIsTracked :: Name -> Symex a Bool
nameIsTracked originalName = variableIsTracked $ makeVarName originalName

-- | Is the name ever used in the program?
nameIsUsed :: Name -> Symex a Bool
nameIsUsed originalName = variableExistsInProgram $ makeVarName originalName

-- | Get the type associated with an LLVMHS name.
-- This name must already be tracked by the backend
getTrackedNameType :: Name -> Symex a Type
getTrackedNameType llvmName = getType $ makeVarName llvmName

-- | Get the type associated with an LLVMHS name,
-- even if it is not already in the backend
getNameType :: Name -> Symex a Type
getNameType llvmName = do
  void $ getDefinedName llvmName
  getTrackedNameType llvmName
