{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import qualified Data.Set as S
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)

type Claim = (Integer, Integer, Integer, Integer, Integer)

claimId :: Claim -> Integer
claimId (i, _, _, _, _) = i

parseClaim :: String -> Claim
parseClaim s = (id, x, y, dx, dy)
  where
    [[id], [x], [y], [dx], [dy]] = (read <$>) <$> s =~ "[0-9]+"

inClaim :: (Integer, Integer) -> Claim -> Bool
inClaim (x, y) (_, x0, y0, dx, dy) = x > x0 && x <= (x0 + dx) && y > y0 && y <= (y0 + dy)

disjoint :: Claim -> Claim -> Bool
disjoint (_, x0, y0, dx0, dy0) (_, x1, y1, dx1, dy1) = x0 + dx0 - 1 < x1 || y0 + dy0 - 1 < y1 || y1 + dy1 - 1 < y0 || x1 + dx1 - 1 < x0

main :: IO ()
main = do
  input <- lines <$> readFile "input3.txt"
  let claims = parseClaim <$> input

  putStrLn "Part 1:"

  let inMoreThanOne = do
        x <- [0..999]
        y <- [0..999]
        guard $ (length $ filter (inClaim (x,y)) claims) > 1
        pure $ (x,y)

  print $ length inMoreThanOne

  putStrLn "Part 2:"

  print $ claimId $ fst $ head $ do
    c <- claims
    let intersects = do
          c' <- claims
          guard $ c' /= c
          guard $ not $ disjoint c c'
          pure c'
    guard  $ null intersects
    pure (c, intersects)
