{-# language ViewPatterns #-}
import Prelude hiding (Word)
import System.IO
import Data.Char
import Data.List (foldl')
import Control.Monad.State
import Control.Monad.List
import Data.List
import System.Random
import Control.Arrow
import System.Environment

import TrieB

type T = CountingTrie Char


type P = Int


data Iter = Open T String | Closed String
data Lines = Lines P [Iter] [Iter]

initial n k t = Lines k [] (replicate n $ (Open t ""))

data Cell = Full Char | Stop


-- todo optimize the checks by count
updateOpponent :: T -> String -> Lines -> Maybe (Lines,T)
updateOpponent t0 xs l@(Lines n ws []) = Just (l,t0)
updateOpponent t0 xs (Lines n ws os) =
  first (Lines n ws . ($ [])) <$> foldM f (id,t0) (zip xs' os) where
    xs' = map Full xs ++ repeat Stop
    f (g,t0) (Full x, Open t s) = (\t -> (g . (:) (Open t (s ++ [x])),t0)) <$> down x t
    f (g,t0) (Stop, Open t s) = case (isPath t && (s `match` t0))  || length s < 3 of
                                  True -> let t0' = maybe t0 id $ deleteCountingPath s t0
                                           in Just (g . (:) (Closed (s)), t0')
                                  False -> Nothing
    f (g,t0) (_,x) = Just (g . (:) x,t0)

update _ t0 ls@(Lines n ws []) ops = return (ls,ops)
update (split -> (g,g')) t0 ls@(Lines n ws (Closed s : os)) ops =
  update g' t0 ops  (Lines undefined (ws ++ [Closed s]) os)
update (split -> (g,g')) t0 (Lines n ws (Open t s : os)) ops = do
  xs <- complete g t
  let   s' = s ++ xs
        s'' = s'

  guard $ (length s' <= n && s' `match` t0)
  let Just t0' = deleteCountingPath s' t0
  case updateOpponent t0' xs ops of
    Nothing -> []
    Just (ops',t0'') -> update g' t0'' ops' (Lines (length s') (ws ++ [Closed s'']) os)

rulePunct x | isLetter x = x
            | otherwise = ' '
k = 6
dict x = do
  ls <- map head <$> group <$> sort <$> map (map toLower) <$> filter (all isLetter) <$> words <$> map rulePunct <$> readFile x
  return $ mkCountingTrie ls

main = do
  (x:xs) <- getArgs
  t <- dict x
  g <- newStdGen
  let rs = update g t (initial 12 12 t) (initial 12 12 t)

  (\f -> foldM f (0,0) rs) $ \n r -> do

    let (Lines _  (map (\(Closed x) -> x) -> cs) _,Lines _ (map (\(Closed x) -> x) -> ls) _) = r
        c = (length  (filter ((>2) .length) $ cs ++ ls) ,sum (map length $ cs ++ ls))
    if c > n then do
      forM_ cs $ putStrLn
      putStrLn "---------------------"
      return c
      else return n


  print $ measure t

example = ["abcde",
           "fgeil",
           "abcde",
           "fghi",
           "ddddd",

           "afafd",
           "bgbgd",
           "cechd",
           "didid",
           "eleld"
          ]
