{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)

margin :: Integer
margin = 1000

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let coords :: [[Integer]] = sort $ (read <$>) <$> splitOn ", " <$> input
  let [x0, y0] = head coords
  let [x1, y1] = last coords
  let closest :: [((Integer, Integer), Maybe Integer)] = do
        x <- [x0-margin..x1+margin]
        y <- [y0-margin..y1+margin]
        let (closest:nextClosest:_) = sort $ do
              (i, [x', y']) <- zip [0..] coords
              let dist = abs (x-x') + abs (y-y')
              pure (dist, i)
        if fst closest == fst nextClosest then
          pure ((x,y), Nothing)
          else
          pure ((x,y), Just $ snd closest)

  let byIndex = groupBy (\a b -> fst a == fst b) $ sort $ do
        ((x,y), indMaybe) <- closest
        i <- maybeToList indMaybe
        pure (i, (x,y))

  print $ length $ last $ sortOn length $ filter (not . onEdge (x0-margin,y0-margin) (x1+margin,y1+margin) ) byIndex

  let foo = do
        x <- [x0-1000 .. x1+1000]
        y <- [y0-1000 .. y1+1000]
        let totDist = sum $ do
              [x',y'] <- coords
              pure $ abs (x-x') + abs (y-y')
        guard $ totDist < 10000
        pure $ (x,y)
  print $ length foo

onEdge :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, (Integer, Integer))] -> Bool
onEdge (x0,y0) (x1, y1) vs = or $ do
  (_, (x, y)) <- vs
  [x == x0, y==y0, x==x1, y==y1]
