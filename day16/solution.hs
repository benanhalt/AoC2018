{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.Bits
import Data.List
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~))

type Registers = V.Vector Int

data Sample = Sample
  { before :: Registers,
    instruction :: [Int],
    after :: Registers
  }
  deriving (Show)

parseSample :: String -> Sample
parseSample s =
  let numbers = read <$> words (filter (`elem` "0123456789 \n") s)
      (before, rest) = splitAt 4 numbers
      (instruction, after) = splitAt 4 rest
   in Sample
        { before = V.fromList before,
          instruction,
          after = V.fromList after
        }

type Operand = Int

type Operation = (String, [Operand] -> Registers -> Registers)

operations :: [Operation]
operations =
  [ ("addr", \[a, b, c] regs -> regs V.// [(c, regs V.! a + regs V.! b)]),
    ("addi", \[a, b, c] regs -> regs V.// [(c, regs V.! a + b)]),
    ("mulr", \[a, b, c] regs -> regs V.// [(c, regs V.! a * regs V.! b)]),
    ("muli", \[a, b, c] regs -> regs V.// [(c, regs V.! a * b)]),
    ("banr", \[a, b, c] regs -> regs V.// [(c, regs V.! a .&. regs V.! b)]),
    ("bani", \[a, b, c] regs -> regs V.// [(c, regs V.! a .&. b)]),
    ("borr", \[a, b, c] regs -> regs V.// [(c, regs V.! a .|. regs V.! b)]),
    ("bori", \[a, b, c] regs -> regs V.// [(c, regs V.! a .|. b)]),
    ("setr", \[a, _, c] regs -> regs V.// [(c, regs V.! a)]),
    ("seti", \[a, _, c] regs -> regs V.// [(c, a)]),
    ("gtir", \[a, b, c] regs -> regs V.// [(c, if a > regs V.! b then 1 else 0)]),
    ("gtri", \[a, b, c] regs -> regs V.// [(c, if regs V.! a > b then 1 else 0)]),
    ("gtrr", \[a, b, c] regs -> regs V.// [(c, if regs V.! a > regs V.! b then 1 else 0)]),
    ("eqir", \[a, b, c] regs -> regs V.// [(c, if a == regs V.! b then 1 else 0)]),
    ("eqri", \[a, b, c] regs -> regs V.// [(c, if regs V.! a == b then 1 else 0)]),
    ("eqrr", \[a, b, c] regs -> regs V.// [(c, if regs V.! a == regs V.! b then 1 else 0)])
  ]

consistent :: Operation -> Sample -> Bool
consistent (name, func) Sample {before, instruction, after} =
  after == func (drop 1 instruction) before

solve :: (Eq a) => [[a]] -> [[a]]
solve ps =
  let (solved, unsolved) = partition ((== 1) . length) ps
   in if null unsolved
        then ps
        else solve $ map (\p -> if length p > 1 then p \\ concat solved else p) ps

execute :: [(Int, Operation)] -> Registers -> [Int] -> Registers
execute opTable regs (opCode : operands) =
  let Just (_, opFunc) = lookup opCode opTable
   in opFunc operands regs

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let samples = parseSample . unlines <$> chunksOf 4 (take 3228 input)
  putStrLn "Part 1:"
  print $ length $ filter (>= 3) $ map (\s -> length $ filter (`consistent` s) operations) samples
  putStrLn "Part 2:"
  let instructions :: [[Int]] = map read . words <$> drop 3230 input
  let possibilities = map (\o -> nub $ map (head . instruction) $ filter (consistent o) samples) operations
  let opTable = zip (concat $ solve possibilities) operations
  print $ foldl' (execute opTable) (V.fromList [0, 0, 0, 0]) instructions
