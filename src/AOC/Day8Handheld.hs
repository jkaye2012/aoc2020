{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day8Handheld (solutionPt1, solutionPt2) where

import AOC.Util
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Attoparsec.ByteString.Char8
import Data.Functor ((<&>))
import qualified Data.Set as S

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show)

isAcc :: Instruction -> Bool
isAcc (Acc _) = True
isAcc _ = False

type Program = [Instruction]

parseInst :: Parser Instruction
parseInst = choice [acc, jmp, nop]
  where
    acc = string "acc " *> signed decimal <&> Acc
    jmp = string "jmp " *> signed decimal <&> Jmp
    nop = string "nop " *> signed decimal <&> Nop

parseProgram :: Parser Program
parseProgram = parseInst `sepBy` endOfLine

type St = (Int, S.Set Int, Int) -- Program counter, processed instructions, accumulator

type Computation = StateT St (Reader Program) (Int, Bool)

runComputation :: Computation
runComputation = do
  (pc, seen, acc) <- get
  prog <- lift ask
  if
      | S.member pc seen -> return (acc, False)
      | pc == length prog -> return (acc, True)
      | otherwise -> do
        let s = S.insert pc seen
        case prog !! pc of
          Nop _ -> put (pc + 1, s, acc)
          Acc val -> put (pc + 1, s, acc + val)
          Jmp offset -> put (pc + offset, s, acc)
        runComputation

solutionPt1 :: String -> IO ()
solutionPt1 s = withParsedInput s parseProgram $ fst . runReader (evalStateT runComputation (0, S.empty, 0))

tryRepair :: Program -> Int -> (Program, Int)
tryRepair prog idx =
  let (h, t) = splitAt idx prog
      u = Prelude.takeWhile isAcc t
      (c : r) = dropWhile isAcc t
      n = case c of
        Nop i -> Jmp i
        Jmp i -> Nop i
   in (h <> u <> (n : r), length h + length u)

type FixSt = (Program, Int) -- Modified program, index of modification

type FixedComputation = StateT FixSt (Reader Program) Int

fixProgram :: FixedComputation
fixProgram = do
  (prog, idx) <- get
  origProg <- lift ask
  let (acc, terminated) = runReader (evalStateT runComputation (0, S.empty, 0)) prog
  if terminated
    then return acc
    else put (tryRepair origProg (idx + 1)) >> fixProgram

solutionPt2 :: String -> IO ()
solutionPt2 s = withParsedInput s parseProgram $ \p -> runReader (evalStateT fixProgram (p, 0)) p
