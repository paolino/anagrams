{-# language ViewPatterns, ImplicitParams #-}
-- | Trie implementation with lexicographically lower path bounded searches

module TrieB where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (..),(<>))
import System.Random
import Data.List
import Data.Ord

-- | A trie holds information in the indexing of children
-- The Bool value indicate a node is also terminal
-- this trie is marked by a monoid, summed up during construction
--
z :: c -> (Map a b -> c) ->Map a b ->  c
z f _ (M.null -> True) = f
z _ g x = g x

data Trie a b
  = Boot
  | Trie !b !Bool !(Map a (Trie a b)) -- ^ a node
  | L
            deriving Show
-- | a path down a trie
type Path a = [a]

mkPath :: (Monoid b, Ord a) => b -> Path a -> Trie a b
mkPath b = foldr f L where
  f x t = Trie b False $ M.singleton x t

mkTrie :: (Monoid b,Ord a) => [(b,Path a)] -> Trie a b
mkTrie = mconcat . map (uncurry mkPath)

instance (Ord a , Monoid b) => Monoid (Trie a b) where

  mempty = Boot
  mappend Boot t = t
  mappend t Boot = t
  mappend L L = L
  mappend (Trie b _ m) L = Trie b True m -- one was terminal (L)
  mappend L (Trie b _ m) = Trie b True m
  mappend (Trie b c m) (Trie b' c' m') = Trie (b <> b') (c || c') $ M.unionWith mappend m m'

measure :: Monoid b => Trie a b -> b
measure (Trie b _ _) = b
measure _ = mempty

down :: Ord a => a -> Trie a b -> Maybe (Trie a b)

down x (Trie _ _ m) = x `M.lookup` m
down _ _ = Nothing

deletePath :: (Ord a,Monoid b)  => (b -> b) -> Path a -> Trie a b -> Maybe (Trie a b)
deletePath f [x] t@(Trie b c m) = let
  g L = Just . z L (Trie (f b) c) $ M.delete x m
  g (Trie b' True m') = Just $ Trie (f b) c (M.insert x (Trie b' False m') m)
  g _ = Nothing
  in M.lookup x m >>= g

deletePath f (x:xs) t@(Trie b c m) = do
  tc <- M.lookup x m
  case tc of
    L -> Nothing
    t'@(Trie _ c' _) -> case (c',deletePath f xs t') of
      (_,Nothing) -> Nothing
      (False,Just L) -> Just $ z  L (Trie (f b) c) $  M.delete x m
      (_,Just t'') -> Just $ Trie (f b) c $ M.insert x t'' m

deletePath _ _ _ = Nothing

match :: Ord a => [a] -> Trie a b -> Bool
match [] L = True
match [] (Trie _ True _) = True
match (x:xs) (Trie _ _ m) = case M.lookup x m of
                              Nothing -> False
                              Just t -> match xs t

match _ _ = False

isPath L = True
isPath (Trie _ True _) = True
isPath _ = False

complete :: (Monoid b, Ord b) => StdGen -> Trie a b -> [[a]]
complete _ Boot = []
complete _ L = [[]]
complete (split -> (s,s')) (Trie _ c m) = (if c then ([]:) else id) $ do
  let rs = randoms s :: [Int]

  (x,t) <- sortBy (flip $ comparing (measure . snd)) $ M.assocs m
  map (x:) $ complete s' t

type CountingTrie a = Trie a (Sum Int)

mkCountingTrie :: Ord a => [Path a] -> CountingTrie a
mkCountingTrie = mkTrie . zip (repeat $ Sum 1)

deleteCountingPath :: Ord a => Path a -> CountingTrie a -> Maybe (CountingTrie a)
deleteCountingPath = deletePath (subtract 1)
