module Main where

import qualified AOC.Day1Stars as Day1
import AOC.Util

main :: IO ()
main = do
  putStr "Day 1 part 1: "
  withPuzzleInput 1 Day1.solutionPt1
  putStr "Day 1 part 2: "
  withPuzzleInput 1 Day1.solutionPt2
