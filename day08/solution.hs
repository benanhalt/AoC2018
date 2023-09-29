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

type Metadata = Int
data Node a = Node [Node a] [a] deriving (Show, Foldable)

parseNode :: [Int] -> (Node Metadata, [Int])
parseNode (0 : nMetadata : rest) = (Node [] (take nMetadata rest), drop nMetadata rest)
parseNode (nChildren : nMetadata : rest) =
  let
    (child, rest') = parseNode rest
    (Node children metadata, rest'') = parseNode (nChildren - 1 : nMetadata : rest')
  in
    (Node (child : children) metadata, rest'')

evaluateNode :: Node Int -> Int
evaluateNode (Node [] metadata) = sum metadata
evaluateNode (Node children metadata) = sum $ do
  i <- metadata
  guard (i > 0)
  child <- take 1 $ drop (i-1) children
  pure $ evaluateNode child

main :: IO ()
main = do
  input <- (map read) <$> words <$> readFile "input.txt"
  let root = fst $ parseNode input
  putStrLn "Part 1:"
  print $ sum root
  putStrLn "Part 2:"
  print $ evaluateNode root
