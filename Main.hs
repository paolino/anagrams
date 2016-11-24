import Data.Char
import qualified Data.Map as M
import Data.List (foldl', sort,intercalate )
import System.IO
import Control.Monad
import Control.Arrow (second)

import Trie

consume xs = let
  ys = sort $ xs
  skip ys x [] = []
  skip ys z (x:xs)  | z == x = skip (x:ys) z xs
                    | otherwise = consume' ys (x:xs)
  consume' ys [x] = [(x,ys)]
  consume' ys (x:xs) = (x,ys ++ xs) : skip (x:ys) x xs
  in consume' [] xs


search :: Ord a => Trie a -> [a] -> [a] -> [([a],[a])]
search L _ xs = return $ (xs,[])
search (Trie False m) _ [] = []
search (Trie True m) _ [] = return ([],[])
search (Trie b m) zs xs = (if b then ((xs,[]):) else id) $ do
  (x,ys) <- consume xs
  let (c,zs') = case zs of
                  [] -> (True,[])
                  (z:zs) -> case compare x z of
                              EQ -> (True,zs)
                              GT -> (True,[])
                              LT -> (False,undefined)

  if c then
    case x `M.lookup` m of
      Nothing -> []
      Just t -> map (second (x:)) $ search t zs' ys
    else []

deepsearch :: Ord a => Trie a -> [a] -> [a] -> [[[a]]]
deepsearch t _ [] = []
deepsearch t zs xs = do
  (rs,ys) <- search t zs xs
  case rs of
    [] -> return [ys]
    rs -> map (ys:) $ deepsearch t ys rs

english = do
  ls <- map (map toLower) <$> lines <$> readFile "words.txt"
  return $ foldl' (flip insert) L $ ls

searchI t = do
  forever $ putStr "> " >> hFlush stdout >> getLine >>= mapM putStrLn . (map $ intercalate " ") . deepsearch t [] . sort . map toLower . filter isLetter

main =  english >>= searchI
