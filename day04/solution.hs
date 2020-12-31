{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE ((=~))
import Debug.Trace (traceShow, traceShowId)

main :: IO ()
main = do
  input <- sort <$> lines <$> readFile "input.txt"

  let shifts = extractShifts input []
  -- print $ head shifts
  -- print $ guardN $ head shifts
  -- print $ transitions $ head shifts
  -- print $ minutes $ transitions $ head shifts

  let fullSummary = foldl' (\m s -> summary s m) M.empty shifts
  let (sleepiest, _) = last $ sortOn snd $ M.toList $ M.map length fullSummary
  -- print sleepiest
  let Just minutesSleeping = M.lookup sleepiest fullSummary
  let mode = head $ last $ sortOn length $ group $ sort minutesSleeping
  -- print mode
  print $ mode * sleepiest

  let fullSum = foldl' (\m s -> summarizeByMin s m) M.empty shifts
  let ((gg, mm), _) = last $ sortOn snd $ M.toList fullSum
  print $ gg * mm

extractShifts :: [String] -> [[String]] -> [[String]]
extractShifts [] shifts = shifts
extractShifts (header:rest) shifts = extractShifts rest' ((header:shift):shifts)
  where
    (shift, rest') = break ("Guard" `isInfixOf`) rest

transitions :: [String] -> [Integer]
transitions shift = (\l -> read $ (head <$> l =~ "[0-9]+") !! 4 ) <$> drop 1 shift

minutes :: [Integer] -> [Integer]
minutes [] = []
minutes (s:e:rest) = [s..e-1] ++ minutes rest

guardN :: [String] -> Integer
guardN (g:_) = read $ (head <$> g =~ "[0-9]+") !! 5

summary :: [String] -> M.Map Integer [Integer] -> M.Map Integer [Integer]
summary shift s = M.insert (guardN shift) ms s
  where
    ms = (minutes $ transitions shift) ++ M.findWithDefault [] (guardN shift) s

summarizeByMin :: [String] -> M.Map (Integer, Integer) Integer -> M.Map (Integer, Integer) Integer
summarizeByMin shift summary = foldl' (\m minute -> M.insert (guardN shift, minute) (1 + M.findWithDefault 0 (guardN shift, minute) m) m) summary ms
  where
    ms = minutes $ transitions shift
