{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.Bits
import Data.List
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~))

type Registers = Vector Int


type Operand = Int

type Operation = (String, [Operand] -> Registers -> Registers)

operations :: [Operation]
operations =
  [ ("addr", \[a, b, c] regs -> regs // [(c, regs ! a + regs ! b)]),
    ("addi", \[a, b, c] regs -> regs // [(c, regs ! a + b)]),
    ("mulr", \[a, b, c] regs -> regs // [(c, regs ! a * regs ! b)]),
    ("muli", \[a, b, c] regs -> regs // [(c, regs ! a * b)]),
    ("banr", \[a, b, c] regs -> regs // [(c, regs ! a .&. regs ! b)]),
    ("bani", \[a, b, c] regs -> regs // [(c, regs ! a .&. b)]),
    ("borr", \[a, b, c] regs -> regs // [(c, regs ! a .|. regs ! b)]),
    ("bori", \[a, b, c] regs -> regs // [(c, regs ! a .|. b)]),
    ("setr", \[a, _, c] regs -> regs // [(c, regs ! a)]),
    ("seti", \[a, _, c] regs -> regs // [(c, a)]),
    ("gtir", \[a, b, c] regs -> regs // [(c, if a > regs ! b then 1 else 0)]),
    ("gtri", \[a, b, c] regs -> regs // [(c, if regs ! a > b then 1 else 0)]),
    ("gtrr", \[a, b, c] regs -> regs // [(c, if regs ! a > regs ! b then 1 else 0)]),
    ("eqir", \[a, b, c] regs -> regs // [(c, if a == regs ! b then 1 else 0)]),
    ("eqri", \[a, b, c] regs -> regs // [(c, if regs ! a == b then 1 else 0)]),
    ("eqrr", \[a, b, c] regs -> regs // [(c, if regs ! a == regs ! b then 1 else 0)])
  ]


data Instruction = Inst String [Operand]
                 -- | BindIP Int
                 deriving (Show)

parseInst :: [String] -> Instruction
--parseInst ["#ip", n] = BindIP (read n)
parseInst (op:operands) = Inst op $ map read operands

data CPU = CPU
  { ip :: !Int,
    regs :: !Registers,
    ipReg :: !Int
  } deriving (Show)

execute :: CPU -> Instruction -> CPU
--execute cpu@CPU {ip, ipReg} (BindIP r) = cpu { ipReg = r, ip = ip +  }
execute cpu@CPU {ip, ipReg, regs} inst@(Inst op operands) =
  let Just operation = lookup op operations
      regs' = operation operands $ regs // [(ipReg, ip)]
  in -- traceShow (cpu, inst, regs') $
  cpu { ip = regs' ! ipReg + 1, regs = regs' }

run :: CPU -> [Instruction] -> CPU
run cpu program | ip cpu < 0 || ip cpu >= length program = cpu
                | otherwise = run (execute cpu $ program !! ip cpu) program

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let program = map (parseInst . words) $ tail input
  putStrLn "Part 1:"
  print $ run CPU { ip = 0, regs = V.fromList [0,0,0,0,0,0], ipReg = 5 } program
  putStrLn "Part 2:"
