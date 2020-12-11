{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module AOC.Day9Codec (solutionPt1, solutionPt2) where

import AOC.Util
import Data.Attoparsec.ByteString.Char8

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = fmap (,x) xs <> pairs xs

preambleSatisfied :: [Int] -> (Bool, Int)
preambleSatisfied [] = (False, 0)
preambleSatisfied lst =
  let target = last lst
      preamble = init lst
      ps = pairs preamble
   in (any (\(x, y) -> x + y == target) ps, target)

firstUnsatisfiedPreamble :: Int -> [Int] -> Int
firstUnsatisfiedPreamble window lst =
  if satisfied
    then firstUnsatisfiedPreamble window (tail lst)
    else curr
  where
    (satisfied, curr) = preambleSatisfied $ Prelude.take (window + 1) lst

findContinuousRangeWithSum :: [Int] -> Int -> [Int]
findContinuousRangeWithSum lst target =
  case go lst target 0 of
    (True, range) -> range
    (False, _) -> findContinuousRangeWithSum (tail lst) target
  where
    go lst target idx =
      let range = Prelude.take idx lst
          acc = sum range
       in if
              | acc == target -> (True, range)
              | acc > target -> (False, [])
              | otherwise -> go lst target (idx + 1)

findEncryptionWeakness :: Int -> [Int] -> Int
findEncryptionWeakness window lst =
  let target = firstUnsatisfiedPreamble window lst
      range = findContinuousRangeWithSum lst target
   in minimum range + maximum range

parser :: Parser [Int]
parser = decimal `sepBy` endOfLine

solutionPt1 :: String -> IO ()
solutionPt1 s = withParsedInput s parser $ firstUnsatisfiedPreamble 25

solutionPt2 :: String -> IO ()
solutionPt2 s = withParsedInput s parser $ findEncryptionWeakness 25
