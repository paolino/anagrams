import Data.Map (Map)
import Data.Char
import qualified Data.Map as M
import Data.List (foldl', tails,sort,intercalate )
-- import Control.Lens
import System.IO
import Control.Monad
import Control.Arrow (second)

consume xs = let
  ys = sort $ xs
  skip ys x [] = []
  skip ys z (x:xs)  | z == x = skip (x:ys) z xs
                    | otherwise = consume' ys (x:xs)
  consume' ys [x] = [(x,ys)]
  consume' ys (x:xs) = (x,ys ++ xs) : skip (x:ys) x xs
  in consume' [] xs

data T a = T Bool (Map a (T a)) | L deriving Show

insert :: Ord a => [a] -> T a -> T a
insert [] L = L
insert [] (T _ m) = T True m
insert (x:xs) L = T False $ M.singleton x $ insert xs L
insert (x:xs) (T b m) = T b $ M.insertWith merge x (insert xs L) m

merge L L = L
merge (T _ m) L = T True m
merge L (T _ m) = T True m
merge (T b m) (T b' m') = T (b || b') $ M.unionWith merge m' m

retrieve L = [[]]
retrieve (T b m) = (if b then ([]:) else id) $ do
  (x,r) <- M.assocs m
  rs <- retrieve r
  return $ x:rs

prefix [] t = Just t
prefix xs L = Nothing
prefix (x:xs) (T b m) = M.lookup x m >>= prefix xs

search :: Ord a => T a -> [a] -> [a] -> [([a],[a])]
search L _ xs = return $ (xs,[])
search (T False m) _ [] = []
search (T True m) _ [] = return ([],[])
search (T b m) zs xs = (if b then ((xs,[]):) else id) $ do
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

deepsearch :: Ord a => T a -> [a] -> [a] -> [[[a]]]
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
