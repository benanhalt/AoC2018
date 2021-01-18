{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns #-}

import Control.Monad (guard)
import Data.List
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)


main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let rules = (\ws -> (ws !! 1, ws !! 7)) <$> words <$> input
  let s = noIncoming rules $ nub $ fst <$> rules
  putStrLn $ concat $ kahn rules s []



-- L ← Empty list that will contain the sorted elements
-- S ← Set of all nodes with no incoming edge

-- while S is not empty do
--     remove a node n from S
--     add n to L
--     for each node m with an edge e from n to m do
--         remove edge e from the graph
--         if m has no other incoming edges then
--             insert m into S

-- if graph has edges then
--     return error   (graph has at least one cycle)
-- else 
--     return L   (a topologically sorted order)

kahn :: [(String, String)] -> [String] -> [String] -> [String]
kahn [] [] l = reverse l
kahn g [] l = error "graph contains cycle"
kahn g s l =
  let
    n = minimum s
    es = filter ((==n) . fst) g
    ms = snd <$> es
    g' = g \\ es
    s' = (noIncoming g' ms) ++ (delete n s)
  in
    kahn g' s' (n:l)

noIncoming :: [(String, String)] -> [String] -> [String]
noIncoming g ns = ns \\ (snd <$> g)
