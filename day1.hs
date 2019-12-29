
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
  input <- readFile "input1.txt"
  let vals = (read . dropWhile (== '+')) <$> lines input :: [Int]
  print $ sum $ vals
  print $ solve (cycle vals) 0 S.empty

solve :: [Int] -> Int -> S.Set Int -> Int
solve (v:vs) prev seen =
  if (v+prev) `elem` seen then (v+prev)
  else solve vs (v+prev) (S.insert (v+prev) seen)

