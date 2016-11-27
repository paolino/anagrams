{-# language ViewPatterns, ImplicitParams, TemplateHaskell #-}
import Data.Char
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List (foldl', sort,intercalate,group,sortBy)
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Lens.TH
import Control.Arrow (second, (&&&))
import Data.Ord
import qualified Data.Set as S
import Data.Monoid

import Trie

type Occ a = Map a Int

mkOcc :: Ord a => [a] -> Map a Int
mkOcc = M.fromList . map (head &&& length) . group . sort

consume :: Ord a => Occ a -> [(a,Occ a)]
consume m = let
  f m k = M.update (\n -> if n > 1 then Just (n - 1) else Nothing) k m
  in map (id &&& f m) $ M.keys m


english = mconcat <$> map (mkPath . map toLower) <$> lines <$> readFile "italians.txt"


data Lang
  = Base String
  | Reset
  | Words
--  | Keep String
--  | Count
--  | MaxWords Int
--  | Dict String
  deriving Read

type Anagrams = [[String]]

data Filter = Filter {
  maxWords :: Maybe Int,
  keepStrings :: [String]
                     }

filterW :: Filter -> Anagrams -> Anagrams
filterW (Filter mw ks) = filter ((&&) <$> (\xs -> all (`elem` xs) ks) <*> (\xs -> maybe True (\n -> length xs <= n) mw))
data InteractionS = InteractionS {
  _filterI :: Filter,
  _anagrams :: Anagrams,
  _trie :: Trie Char
  }

makeLenses ''InteractionS

type InteractionM = StateT InteractionS IO

filter0 = Filter Nothing []
state0 = InteractionS filter0 [] <$> english

interpret :: (?conf :: Search (Occ Char) Char) => Lang -> InteractionM ()

interpret Reset = modify (set filterI filter0)

interpret (Base l) = do
  interpret Reset
  modify (\s -> set anagrams
    ( iterateSearch (view trie s) [] $ mkOcc . map toLower . filter isLetter $ l) s)
  xs <- gets $ view anagrams
  liftIO $ print (length xs)

interpret Words = do
  ws <- gets $ view anagrams
  liftIO $ forM_ (S.toList . S.fromList $ concat ws) $ \w -> putStr (w ++ " ")
  liftIO $ putStrLn mempty

-- interpret (Keep l) =


prefix [] t = True
prefix _ L = False
prefix (x:xs) (Trie _ m) = case M.lookup x m of
                          Nothing -> False
                          Just t -> prefix xs t



main = do
  -- let ?conf = Search M.null consume
      {-
  state0 >>= \t -> flip evalStateT t $ forever $ do
    liftIO $ putStr "> " >> hFlush stdout
    liftIO readLn >>= interpret
    -}
  t <- english
  forever $ getLine >>= print . flip prefix t

