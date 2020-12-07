module Main where

import qualified AOC.Day1Stars as Day1
import qualified AOC.Day2Passwords as Day2
import qualified AOC.Day3Trees as Day3
import qualified AOC.Day4Passports as Day4
import qualified AOC.Day5Seats as Day5
import qualified AOC.Day6Customs as Day6
import AOC.Util

main :: IO ()
main = do
  putStr "Day 1 part 1: "
  withPuzzleInput 1 Day1.solutionPt1
  putStr "Day 1 part 2: "
  withPuzzleInput 1 Day1.solutionPt2
  putStr "Day 2 part 1: "
  withPuzzleInput 2 Day2.solutionPt1
  putStr "Day 2 part 2: "
  withPuzzleInput 2 Day2.solutionPt2
  putStr "Day 3 part 1: "
  withPuzzleInput 3 Day3.solutionPt1
  putStr "Day 3 part 2: "
  withPuzzleInput 3 Day3.solutionPt2
  putStr "Day 4 part 1: "
  withPuzzleInput 4 Day4.solutionPt1
  putStr "Day 4 part 2: "
  withPuzzleInput 4 Day4.solutionPt2
  putStr "Day 5 part 1: "
  withPuzzleInput 5 Day5.solutionPt1
  putStr "Day 5 part 2: "
  withPuzzleInput 5 Day5.solutionPt2
  putStr "Day 6 part 1: "
  withPuzzleInput 6 Day6.solutionPt1
  putStr "Day 6 part 2: "
  withPuzzleInput 6 Day6.solutionPt2
