{-|

This module is the static uninit memory checker. Its symbolic counterpart is in
UninitSymbolic.hs.

-}
module Checkers.UninitStatic ( uninitCheck
                             , blankUninit
                             , UninitCheckType(..)
                             , Uninits
                             , UninitBug(..)
                             ) where
import           Checkers.Utils.StaticUtils
import           Control.Monad              (forM_, unless, when)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes)
import qualified Data.Set                   as S
import           LLVM.AST                   hiding (args)
import qualified LLVM.AST.Operand           as O
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getFunName)
import           Static.CheckerState

-- | Identify suspicious-looking code snippets that might be uses of uninitialized memory.
-- The checker works for both heap uninitalized and stack unitialized bugs, but the paper
-- only describes that stack uninit checker.
-- The checker matches on each LLVM instruction ('ninstr') to update the internal state.
uninitCheck :: UninitCheckType
            -> Int
            -> Named Instruction
            -> Checker Uninits UninitBug ()
uninitCheck checkType lineno ninstr = do
  maybeReportBug ninstr lineno
  filterAsm ninstr >> filterFloats ninstr >> filterAsserts ninstr
  modelAssigns ninstr

  case ninstr of

    -- If its freshly allocated, add to uininit set
    -- Don't do this for vectors because they are a special case
    name := Alloca ty _ _ _ | isStack checkType && (not $ isVector ty) -> uninitVar name
    name := Call _ _ _ co _ _ _ | "malloc" `isInfixOf` show co
                                && isHeap checkType -> uninitVar name

    -- If the cast thing or the GEP base is uninit, mark the rhs uninit
    name := BitCast var _ _         -> maybeUninitVar var name
    name := GetElementPtr _ addr _ _ -> do
      -- gep 0 0 alias base, gep 0 %v alias base
      when (allZerosOrLoop ninstr) $ addAlias name addr
      maybeUninitVar addr name

    -- If it's a call, remove all pointer arguments from the uninit set
    _ | (Call _ _ _ co args _ _) <- getInstruction ninstr -> do
      unless (isLLVMAnnot co) $ do
        forM_ args $ \argInfo -> do
          let arg = fst argInfo
              attrs = snd argInfo
          unless (isReadOnly attrs) $ initVar arg

    -- If it's stored to an addr or a store, remove from uninit set
    Do (Store _ addr val _ _ _)  | isStack checkType -> initVar addr >> initVar val
    -- If it's ptrToInted, remove from uninit set
    _ := PtrToInt ptr _ _ -> initVar ptr

    -- If it's a load, add it to the may-be-a-bug set
    name := Load _ addr _ _ _ -> do
      uninit <- isUninit addr
      when uninit $ maybeBug addr name

    -- If there are no more possibly uninit variables, there can be no more bugs.
    _ -> when (isStack checkType) $ do
           s0 <- getState
           when (null $ uninits s0) $ die

vectorRelated :: Type -> Bool
vectorRelated VectorType{}      = True
vectorRelated (PointerType p _) = vectorRelated p
vectorRelated _                 = False

allZerosOrLoop :: Named Instruction -> Bool
allZerosOrLoop ninstr =
  case ninstr of
    _ := GetElementPtr _ _ is _ ->
      all isZero is || isZero (head is) && all (not . isConst) (tail is)
    _                           -> False

-- Figuring out whats a bug
-- Empirically, basically everything's not a bug that's a select or optional type (& -> store)
-- Also empirically, none of our bugs (or the many clang ones ive looked at) have these
-- characteristics. As such, we will suppress based on the bad news ops instead of the
-- good news ops

maybeBug :: Operand -> Name -> Checker Uninits UninitBug ()
maybeBug op name = do
  s0 <- getState
  putState $ s0 { tainted = M.insert op (S.singleton name) $ tainted s0 }

maybeBitfield :: Name -> Operand -> Checker Uninits UninitBug ()
maybeBitfield newName oldOp = do
  s0 <- getState
  putState $ s0 { tainted   = M.delete oldOp $ tainted s0
                , bitfields = S.insert newName $ bitfields s0
                }

maybeReportBug :: Named Instruction -> Int -> Checker Uninits UninitBug ()
maybeReportBug ni lineno = do
  s0 <- getState
  let allTainted = tainted s0
      allFields  = bitfields s0
  unless (null allTainted) $ do
    let names = catMaybes $ map nameOf $ getOperands ni
    -- Go through all of the bitfields and see if they're stored back
    forM_ allFields $ \field -> do
      let matches = filter (\n -> Just field == nameOf n) $ getOperands ni
      case matches of
        [f] -> case ni of
          Do (Store{}) -> die
          _            -> report (nameOf' f) (typeOf f)
        _ -> return ()
    -- Go through everything that's tainted to see if it's used in a bad way
    forM_ (M.toList allTainted) $ \(key, vals) -> do
      when (any (\name -> S.member name vals) names) $ do
        case ni of
          -- The compiler is doing
          _             | vectorRelated $ typeOf key -> die
          -- This is short circuiting, typically
          _ := Select{} -> die
          -- This could be initializing a bitfield
          n := And{}    -> maybeBitfield n key
          n := Or{}     -> maybeBitfield n key
          -- Otherwise it's usually bad
          _             -> report (nameOf' key) (typeOf key)
  where report name ty = do
          path <- getPath
          fun <- getFunName
          file <- getFilepath
          addBug $ UninitBug lineno path [] fun file name ty

-- Figuring out whats uninit

maybeUninitVar :: Operand -> Name -> Checker Uninits UninitBug ()
maybeUninitVar op name = do
  uninit <- isUninit op
  when uninit $ uninitVar name

uninitVar :: Name -> Checker Uninits UninitBug ()
uninitVar name = do
  s0 <- getState
  putState $ s0 { uninits = S.insert name $ uninits s0 }

initVar :: Operand -> Checker Uninits UninitBug ()
initVar (O.LocalReference _ name) = do
  aliases <- getAliasesOf name
  s0 <- getState
  putState $ s0 { uninits = S.difference (uninits s0) aliases }
initVar _ = return ()

isUninit :: Operand -> Checker Uninits UninitBug Bool
isUninit (O.LocalReference _ name) = do
  aliases <- getAliasesOf name
  s0 <- getState
  return $ not $ null $ S.intersection aliases $ uninits s0
isUninit _ = return False

blankUninit :: Uninits
blankUninit = Uninits S.empty M.empty S.empty

data Uninits = Uninits { uninits   :: S.Set Name
                       , tainted   :: M.Map Operand (S.Set Name)
                       , bitfields :: S.Set Name
                       }
             deriving (Eq, Ord, Show)

isStack, isHeap :: UninitCheckType -> Bool
isStack Stack = True
isStack _     = False
isHeap Heap = True
isHeap _    = False

data UninitCheckType = Stack | Heap
                       deriving (Eq, Ord, Show)

data UninitBug = UninitBug Int Path [Name] Name String Name Type
               deriving (Eq, Ord, Show)
