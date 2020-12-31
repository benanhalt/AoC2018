{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)

main :: IO ()
main = do
  input <- head <$> lines <$> readFile "input.txt"
  print $ length $ contractFull input
  print $ head $ sort $ do
        c <- chr <$> [ord 'a' .. ord 'z']
        pure $ length $ contractFull $ filter ((/= c) . toLower) input

flipCase :: Char -> Char
flipCase c | isAsciiUpper c = toLower c
           | otherwise = toUpper c

contract :: String -> String -> String
contract result ""  = reverse result
contract result [c] = contract (c:result) []
contract result (c:d:rest) =
  if flipCase c == d
  then contract result rest
  else contract (c:result) (d:rest)

contractFull :: String -> String
contractFull s = if contracted == s then s else contractFull contracted
  where contracted = contract "" s

