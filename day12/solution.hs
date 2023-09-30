{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, DeriveFoldable #-}

import Control.Monad (guard)
import Data.List
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)

parseRule :: String -> (Int, Bool)
parseRule s =
  ( foldl' (\a b -> 2*a + b) 0 $ map (\c -> if c == '#' then 1 else 0) $ take 5 s
  , last s == '#'
  )

step :: S.Set Int -> S.Set Int -> S.Set Int
step rules state =
  let
    left = S.findMin state - 2
    right = S.findMax state + 2
  in S.fromList $ do
    i <- [left .. right]
    let pat = sum $ do
          (m, j) <- zip [16,8,4,2,1] [i-2 .. i+2]
          guard $ j `S.member` state
          pure m
    guard $ pat `S.member` rules
    pure i

display :: S.Set Int -> String
display state =
  let
    left = S.findMin state
    right = S.findMax state
    graph = do
      i <- [left .. right]
      pure $ if i `S.member` state then '#' else '.'
   in show left ++ graph

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let initState = (words $ head input) !! 2
  let state0 = S.fromList $ map fst $ filter ((== '#') . snd) $ zip [0..] initState
  let rules = S.fromList $ map fst $ filter snd $ map parseRule $ drop 2 input
  putStrLn "Part 1:"
  let trajectory = iterate (step rules) state0
  print $ sum $ trajectory !! 20
  putStrLn $ unlines $ map display $ take 20 trajectory
  putStrLn "Part 2:"


