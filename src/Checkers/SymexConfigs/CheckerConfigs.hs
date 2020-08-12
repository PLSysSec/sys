module Checkers.SymexConfigs.CheckerConfigs where
import           Checkers.ConcreteOOBSymbolic
import           Checkers.HeapOOBSymbolic
import           Checkers.UAFSymbolic
import           Checkers.UninitSymbolic
import           Checkers.UserInputSymbolic
import qualified Data.Map                     as M
import           LLVM.AST
import           Prelude                      hiding (pred)
import           Prelude                      hiding (pred)
import           Symex.CheckerConfigDef
import           Symex.Symex.Symex


type DummyState = String

-- | No attacks, just symbolically execute the path
staticPathConfig :: Config DummyState
staticPathConfig = testConfig { pathLength = 10
                              , timeout = 1200
                              , verbose = True
                              }

concroobSymexConfig :: (Either Name Integer) -> Integer -> Config DummyState
concroobSymexConfig indexVar indexAmt =
  testConfig { symexRefinements = [negRefine indexVar indexAmt]
             , pathLength = 20
             , oobOk = True
             , verbose = True
             }

heapoobSymexConfig :: Either Name Int
                   -> Either Name Int
                   -> Int
                   -> Bool
                   -> Config DummyState
heapoobSymexConfig allocSize indexSize jumpSize ig =
  staticPathConfig { postAttacks = [innerObj]
                   , symexRefinements = [mayOOB allocSize indexSize jumpSize ig]
                   , oobOk = True
                   , verbose = True
                   }

uninitSymexConfig :: Name -> Type -> Config SizeInfo
uninitSymexConfig name ty =
  Config { symexContents = const True
         , symexAST = const True
         , symexDefn = const True
         , symexPath = const True
         , canAlias = False
         , memoryLayout = True
         , callsOn = False
         , returnFollowOn = False
         , symexShadow = True
         , outputResult = const $ return ()
         , symexRefinements = [uninitRefine name ty]
         , pathTransform = id
         , pathLength = 5
         , arrayBound = 200
         , oobOk = True
         , timeout = 20
         , preAttack = Nothing
         , postAttacks = [unconstrainUninitCall]
         , initializer = setAttackerState $ SizeInfo M.empty M.empty
         , verbose = True
         }

userInputSymexConfig :: Name -> Int -> Config Bool
userInputSymexConfig var val =
  Config { symexContents = const True
         , symexAST = const True
         , symexDefn = const True
         , symexPath = const True
         , canAlias = False
         , memoryLayout = True
         , callsOn = False
         , returnFollowOn = False
         , symexShadow = False
         , outputResult = const $ return ()
         , symexRefinements = [userInputRefine var val]
         , pathTransform = id
         , pathLength = 20
         , arrayBound = 20
         , oobOk = True
         , timeout = 60
         , preAttack = Nothing
         , postAttacks = []
         , initializer = setAttackerState False
         , verbose = True
         }

uafSymexConfig :: Name -> Type -> Config Bool
uafSymexConfig _ ty =
  Config { symexContents = const True
         , symexAST = const True
         , symexDefn = const True
         , symexPath = const True
         , canAlias = False
         , memoryLayout = True
         , callsOn = False
         , returnFollowOn = False
         , symexShadow = True
         , outputResult = const $ return ()
         , symexRefinements = []
         , pathTransform = id
         , pathLength = 20
         , arrayBound = 20
         , oobOk = True
         , timeout = 60
         , preAttack = Nothing
         , postAttacks = [uafPostAttack ty]
         , initializer = setAttackerState False
         , verbose = True
         }


-- | For the backend tests
testConfig :: Config DummyState
testConfig =
  Config { symexContents = const True
         , symexAST = const True
         , symexDefn = const True
         , symexPath = const True
         , canAlias = False
         , memoryLayout = True
         , callsOn = False
         , returnFollowOn = False
         , symexShadow = False
         , outputResult = const $ return ()
         , symexRefinements = []
         , pathTransform = id
         , pathLength = 1
         , arrayBound = 20
         , oobOk = False
         , timeout = 5
         , preAttack = Nothing
         , postAttacks = []
         , initializer = return ()
         , verbose = False
         }
