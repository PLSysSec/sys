-- | Checker configuration definition
module Symex.CheckerConfigDef where
import           Checkers.Attack
import           Data.ByteString       (ByteString)
import           InternalIR.SimplePath (SimplePath)
import           LLVM.AST
import           Prelude               hiding (pred)
import           Prelude               hiding (pred)
import           Symex.Symex.Symex

data Config a = Config {
    -- | Should we symbolically execute this text?
    symexContents    :: ByteString -> Bool,
    -- | Should we symex this AST?
    symexAST         :: Module -> Bool,
    -- | Should we symex this function definition?
    symexDefn        :: Definition -> Bool,
    -- | Shoule we symex this path?
    symexPath        :: SimplePath -> Bool,
    -- | Should we assume that arguments in this function can alias?
    canAlias         :: Bool,
    -- | Should we use strict memory layout?
    memoryLayout     :: Bool,
    -- | Should we enter calls?
    callsOn          :: Bool,
    -- | Should we follow returns to callsites?
    returnFollowOn   :: Bool,
    -- | Should we use shadow mem?
    symexShadow      :: Bool,
    -- | Function to display the result
    -- Includes the basic block parameter so we can display what path we've symed
    outputResult     :: SimplePath -> Symex a (),
    -- | Attack-related refinements to run right before solving
    symexRefinements :: [Symex a ()],
    -- | Arbitrary transformation to apply to the path before symex
    pathTransform    :: SimplePath -> SimplePath,
    -- | Number of basic blocks to symex over
    pathLength       :: Int,
    -- | Assume all heap allocated arrays have no more than length arrayBound
    arrayBound       :: Integer,
    -- | Are OOBs to be allowed by symex?
    oobOk            :: Bool,
    -- | Timeout for the solver
    timeout          :: Integer,
    -- | Attacks to run before the system automatically executes the instruction
    -- Can tell framework to skip instruction
    preAttack        :: Maybe (PreAttack a),
    -- | Attacks to run after the system has automatically executed the instruction
    postAttacks      :: [PostAttack a],
    -- | Initializer to run before symex
    initializer      :: Symex a (),
    -- | Verbose output y/n
    verbose          :: Bool
    }

