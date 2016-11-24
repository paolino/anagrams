{-# language ViewPatterns, ImplicitParams #-}
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (foldl', sort,intercalate,group)
import System.IO
import Control.Monad
import Control.Arrow (second, (&&&))

import Trie

type Occ a = Map a Int

mkOcc :: Ord a => [a] -> Map a Int
mkOcc = M.fromList . map (head &&& length) . group . sort

consume :: Ord a => Occ a -> [(a,Occ a)]
consume m = let
  f m k = M.update (\n -> if n > 1 then Just (n - 1) else Nothing) k m
  in map (id &&& f m) $ M.keys m


english = do
  ls <- map (map toLower) <$> lines <$> readFile "words.txt"
  return $ foldl' (flip insert) L $ ls

searchI t = do
  let ?conf = Search M.null consume
  forever $ do
      putStr "> " >> hFlush stdout
      l <- mkOcc <$> map toLower <$> filter isLetter <$> getLine
      mapM putStrLn . (map $ intercalate " ") . iterateSearch t [] $ l

main =  english >>= searchI
