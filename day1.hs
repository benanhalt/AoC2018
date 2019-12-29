


main :: IO ()
main = do
  input <- readFile "input1.txt"
  let vals = (read . dropWhile (== '+')) <$> lines input :: [Int] 
  print $ sum $ vals
