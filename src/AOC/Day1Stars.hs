{-# LANGUAGE TupleSections #-}

module AOC.Day1Stars (solutionPt1, solutionPt2) where

type Input = [Int]

parseInput :: String -> Input
parseInput = fmap read . lines

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = fmap (,x) xs <> pairs xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples (x : xs) =
  let p = pairs xs
   in fmap (\(a, b) -> (a, b, x)) p <> triples xs

solutionPt1 :: String -> IO ()
solutionPt1 s =
  let input = pairs . parseInput $ s
      (a, b) = head $ filter (\(a, b) -> a + b == 2020) input
      result = a * b
   in print result

solutionPt2 :: String -> IO ()
solutionPt2 s =
  let input = triples . parseInput $ s
      (a, b, c) = head $ filter (\(a, b, c) -> a + b + c == 2020) input
      result = a * b * c
   in print result
