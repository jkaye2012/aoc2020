module AOC.Day10Jolts (solutionPt1, solutionPt2) where

import AOC.Util
import Data.Attoparsec.ByteString.Char8
import Data.List (group, partition, sort)

joltDifferences :: [Int] -> Int
joltDifferences jolts =
  let (ones, threes) = partition (== 1) . filter (`elem` [1, 3]) $ zipWith (-) (tail jolts) jolts
   in length ones * length threes

parser :: Parser [Int]
parser = do
  adapters <- decimal `sepBy` endOfLine
  let sorted = sort adapters
  return $ [0] <> sorted <> [3 + last sorted]

solutionPt1 :: String -> IO ()
solutionPt1 s = withParsedInput s parser joltDifferences

numConfigs :: [Int] -> Int
numConfigs l
  | length l == 1 = 2
  | length l == 2 = 4
  | length l == 3 = 7

validConfigurations :: [Int] -> Int
validConfigurations jolts =
  let d = zipWith (-) (tail jolts) jolts
      dp = filter (\l -> 2 `elem` l) . group $ zipWith (+) d (tail d)
   in product (numConfigs <$> dp)

solutionPt2 :: String -> IO ()
solutionPt2 s = withParsedInput s parser validConfigurations
