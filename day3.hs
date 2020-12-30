{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)


parseClaim :: String -> [Integer]
parseClaim s = read <$> head <$> s =~ "[0-9]+"

main :: IO ()
main = do
  input <- lines <$> readFile "input3.txt"
  let claims = parseClaim <$> input

  putStrLn "Part 1:"

  let countAll = foldl' count1 M.empty claims
  print $ length $ filter (> 1) $ M.elems countAll

  putStrLn "Part 2:"
  print $ head $ do
    [i, x, y, dx, dy] <- claims
    guard $ and $ do
      x' <- [x .. x+dx-1]
      y' <- [y .. y+dy-1]
      pure $ Just 1 == M.lookup (x',y') countAll
    pure i

count1 :: M.Map (Integer,Integer) Integer -> [Integer] -> M.Map (Integer, Integer) Integer
count1 m [i, x, y, dx, dy] = foldl' (\m coord -> M.insert coord (1 + M.findWithDefault 0 coord m) m) m $ do
  x' <- [x .. x+dx-1]
  y' <- [y .. y+dy-1]
  pure (x',y')


