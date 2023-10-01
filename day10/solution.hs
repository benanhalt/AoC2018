{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~))

parseLine :: String -> [Double]
parseLine s = read <$> words (filter (`elem` " -0123456789") s)

update :: [Double] -> [Double]
update [x, y, vx, vy] = [x + vx, y + vy, vx, vy]

extent :: [Double] -> [[Double]] -> Double
extent (cx : cy : _) points = sum $ map (\(x : y : _) -> (x - cx) * (x - cx) + (y - cy) * (y - cy)) points

display :: [[Double]] -> String
display coords =
  let points = map (take 2) coords
      maxX = maximum $ map (!! 0) points
      maxY = maximum $ map (!! 1) points
      minX = minimum $ map (!! 0) points
      minY = minimum $ map (!! 1) points
   in unlines $ do
        y <- [minY - 1 .. maxY + 1]
        pure $ do
          x <- [minX - 1 .. maxX + 1]
          pure $ if [x, y] `elem` points then '*' else ' '

main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  let n = fromIntegral $ length input
  let center = map (/ n) $ foldl' (zipWith (+)) [0, 0, 0, 0] input
  let trajectory = iterate (map update) (center : input)
  let extents = map (\(center : points) -> extent center points) trajectory
  let deltaExtent = zipWith (-) extents (drop 1 extents)
  let (before, after) = break ((< 0) . fst) $ zip deltaExtent trajectory
  let (_, _ : solution) = head after
  putStrLn $ display solution
  putStrLn "Part 2:"
  print $ length before
