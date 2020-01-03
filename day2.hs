import Control.Monad
import Data.List

main = do
  input <- readFile "input2.txt"
  let ids = lines input
  let repeats = ((fmap length) . group . sort) <$> ids
  let twos = filter (any (== 2)) repeats
  let threes = filter (any (== 3)) repeats
  print $ length twos * length threes
  print $ solve ids


solve :: [String] -> [(String, String)]
solve ids = do
  a <- ids
  b <- ids
  let differing = filter (== False) $ zipWith (==) a b
  guard $ length differing == 1
  pure (a,b)
