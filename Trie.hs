module Trie where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl', tails,sort,intercalate )


data Trie a = Trie Bool (Map a (Trie a)) | L deriving Show

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] L = L
insert [] (Trie _ m) = Trie True m
insert (x:xs) L = Trie False $ M.singleton x $ insert xs L
insert (x:xs) (Trie b m) = Trie b $ M.insertWith merge x (insert xs L) m

merge L L = L
merge (Trie _ m) L = Trie True m
merge L (Trie _ m) = Trie True m
merge (Trie b m) (Trie b' m') = Trie (b || b') $ M.unionWith merge m' m


