{- | Simple module for get bounded path prefixes. This is useful when we want
 - to start a little bit before a particular basic block. -}
module Utils.Pathsearch.PathPrefix ( PathPrefixEntry
                                   , PathPrefix
                                   , getAllPathPrefixesWithSuffix) where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set
import Data.Map (Map)
import qualified Data.Map as Map
import LLVM.AST
import LLVMAST.ASTInterface
import InternalIR.ModuleInfo
import Utils.Pathsearch.BlockPredicate

-- | A path prefix is a list of (module name, function name, basic-block name)'s
-- The path prefix should never be the empty list.
type PathPrefixEntry = (String, Name, Name)
type PathPrefix = [PathPrefixEntry]

-- | Map of basic block -> prev basic blocks
type RevMap = Map Name (Set Name)

-- | Create a reverse map for a particular function
functionRevMap :: Name -> Module -> RevMap
functionRevMap fName modast =
  let allBBs = fromMaybe [] $ bbsFromFunctionName modast fName
  in foldr addBlockToRevMap Map.empty allBBs

-- | Given basic block, add the reverse maping to the current map
addBlockToRevMap :: BasicBlock -> RevMap -> RevMap
addBlockToRevMap bb rMap0 =
  let nextBBs = getNextBlocksOf bb
  in foldr (\nextBB rMap -> Map.alter doAdd nextBB rMap) rMap0 nextBBs
  where doAdd Nothing    = Just $ Set.singleton curBB
        doAdd (Just bbs) = Just $ Set.insert curBB bbs
        curBB  = getBlockName bb

-- | Get all path prefixes of given length (up to function entry) that end with
-- a particular property. This function does NOT enter calls.
--
-- NOTE: for now we may produce prefixes that seem redundant. A prefix may be
-- redundant if there is another prefix that is a prefix of it. This happens
-- when we have loops near the function entry.  For exmaple, we may produce the
-- following prefix with length set to 4:
--
-- > [[1, 2], [1, 2, 2], [1, 2, 2, 2]]
--
-- All but the first may seem redundant. But, the last one of length 4 may be
-- most useful. I'm not sure, so for now leaving all.
getAllPathPrefixesWithSuffix :: Name
                             -- ^ Function name
                             -> ModuleInfo
                             -- ^ LLVM Module + precomputed module info
                             -> Int
                             -- ^ How long should the path be?
                             -> BlockPredicate
                             -- ^ Predicate for which the last block of the
                             -- path must be true
                             -> Set PathPrefix
getAllPathPrefixesWithSuffix fName mInfo len bPred =
  Set.filter isOK $
    Set.concatMap (getPrefixesFor mName fName revMap len) endBBs
    where mName = modName mInfo
          -- All the function basic blocks:
          allBBs = fromMaybe [] $ bbsFromFunctionName (modAST mInfo) fName
          -- All the viable "end" blocks:
          endBBs = Set.fromList $ map getBlockName $ filter (isTrue bPred mInfo) allBBs
          -- Reverse map
          revMap = functionRevMap fName $ modAST mInfo
          -- Prefix is okay if it's of the right length or ends at function
          -- entry.
          isOK prefix = length prefix == len ||
                       (third $ head prefix) == (getBlockName $ head allBBs)
          third (_,_,c) = c



-- | Get all the prefixes that end in a particular block.
getPrefixesFor :: String          -- ^ Module name
               -> Name            -- ^ Function name
               -> RevMap          -- ^ All basic blocks
               -> Int             -- ^ Max length of path
               -> Name            -- ^ End block
               -> Set PathPrefix  -- ^ Basic block paths
getPrefixesFor mName fName _      n   endBB | n <= 1 = Set.singleton [(mName, fName, endBB)]
getPrefixesFor mName fName revMap len endBB =
  case Map.lookup endBB revMap of
    Nothing -> Set.singleton [(mName, fName, endBB)]
    Just prevBlocks -> Set.concatMap (\prevBlock ->
                          Set.map (++[(mName, fName, endBB)]) $ getPrefixesFor mName fName revMap (len - 1) prevBlock
                       ) prevBlocks
