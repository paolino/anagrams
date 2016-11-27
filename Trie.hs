{-# language ViewPatterns, ImplicitParams, NoMonomorphismRestriction #-}
-- | Trie implementation with lexicographically lower path bounded searches

module Trie where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Control.Arrow (second)
import Control.Monad (guard)


-- | A trie holds information in the indexing of children
-- The Bool value indicate a node is also terminal
data Trie a = Trie !Bool !(Map a (Trie a)) -- ^ a node
            | L  -- ^ an end of path

instance (Ord a) => Monoid (Trie a) where
  mempty = L
  mappend L L = L
  mappend (Trie _ m) L = Trie True m -- one was terminal (L)
  mappend L (Trie _ m) = Trie True m
  mappend (Trie b m) (Trie b' m') = Trie (b || b') $ M.unionWith mappend m' m

-- | a path down a trie
type Path a = [a]

mkPath :: Ord a => Path a -> Trie a
mkPath = foldr f mempty where
  f x t = Trie False $ M.singleton x t

mkTrie = mconcat . map mkPath


-- searching library ----------------------
--
--
-- | a configuration for a search , 'm' is the search
-- state 'a' is the indexing in the trie
data Search m a = Search {
  empty :: m -> Bool, -- ^ check if the search is over
  expand :: m -> [(a,m)] -- ^ produce choices from a search state
                         }

-- | search through a trie until a leaf is reached.
search  :: (Ord a,?conf :: Search m a)

        => Trie a -- ^ trie
        -> Path a -- ^ lower bound (lexicographically) for the paths
        -> m -- ^ initial search state
        -> [(m,Path a)] -- ^ all found leaves coupled with the search state reminder
search L _ xs = return $ (xs,[])
search (Trie b m) _ h@(empty ?conf -> True)  = if b then return (h,[]) else []
search (Trie b m) zs h = (if b then ((h,[]):) else id) $ do

  (x,ys) <- expand ?conf h -- next steps

  let (c,zs') = -- matching lexicographically lower bound
        case zs of
          [] ->  (True,[]) -- no problem we are lessicographically higher
          (z:zs) -> case compare x z of
                      EQ -> (True,zs) -- we are matching, check on next level
                      GT -> (True,[]) -- we are higher from now on
                      LT -> (False,undefined) -- we are lower, don't go on

  guard c
  case x `M.lookup` m of
      Nothing -> []
      Just t -> map (second (x:)) $ search t zs' ys

-- | produces all sequences of paths, which consume all the search state
iterateSearch  :: (Ord a, ?conf :: Search m a)
               => Trie a -- ^ trie
                -> Path a -- ^ lower bound path
                -> m -- ^ initial search state
                -> [[Path a]] -- ^ sequences
iterateSearch t _ ( empty ?conf -> True) = []
iterateSearch t zs xs = do
  (rs,ys) <- search t zs xs
  case rs of
    (empty ?conf -> True) -> return [ys]
    rs -> map (ys:) $ iterateSearch t ys rs

