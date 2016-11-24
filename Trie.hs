{-# language ViewPatterns, ImplicitParams #-}
module Trie where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List ()
import Control.Arrow (second)


-- | A trie holds information in the indexing of children
-- The Bool value indicate a node is also terminal
data Trie a = Trie Bool (Map a (Trie a)) | L deriving Show

-- insertion --------------------
--
-- | insert a path
insert :: Ord a => [a] -> Trie a -> Trie a
insert [] L = L
insert [] (Trie _ m) = Trie True m -- set the node as terminal
insert (x:xs) L = Trie False $ M.singleton x $ insert xs L
insert (x:xs) (Trie b m) = Trie b $ M.insertWith merge x (insert xs L) m

-- | merge 2 tries
merge L L = L
merge (Trie _ m) L = Trie True m -- one was terminal (L)
merge L (Trie _ m) = Trie True m
merge (Trie b m) (Trie b' m') = Trie (b || b') $ M.unionWith merge m' m


-- searching library ----------------------
--
--
-- | a configuration for a search , 'm' is the search state 'a' is the indexing in the trie
data Search m a = Search {
  empty :: m -> Bool, -- check if the search is over
  expand :: m -> [(a,m)] -- produce choices from a search state
                         }

-- | search through a trie until a leaf is reached.
search  :: (Ord a,?conf :: Search m a)

        => Trie a -- ^ trie
        -> [a] -- ^ lower bound (lexicographically) for the paths
        -> m -- ^ initial search state
        -> [(m,[a])] -- all found leaves
search L _ xs = return $ (xs,[])
search (Trie b m) _ h@(empty ?conf -> True)  = if b then return (h,[]) else []
search (Trie b m) zs h = (if b then ((h,[]):) else id) $ do

  (x,ys) <- expand ?conf h -- next steps

  let (c,zs') = -- matching lexicographically successor
        case zs of
          [] ->  (True,[]) -- no problem we are lessicographically higher
          (z:zs) -> case compare x z of
                      EQ -> (True,zs) -- we are matching, check on next level
                      GT -> (True,[]) -- we are higher from now on
                      LT -> (False,undefined) -- we are lower, don't go on

  if c then
    case x `M.lookup` m of
      Nothing -> []
      Just t -> map (second (x:)) $ search t zs' ys
    else []

completeSearch  :: (Ord a, ?conf :: Search m a)
                => Trie a
                -> [a]
                -> m
                -> [[[a]]]
completeSearch t _ ( empty ?conf -> True) = []
completeSearch t zs xs = do
  (rs,ys) <- search t zs xs
  case rs of
    (empty ?conf -> True) -> return [ys]
    rs -> map (ys:) $ completeSearch t ys rs

