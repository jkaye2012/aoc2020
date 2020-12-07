{-# LANGUAGE OverloadedStrings #-}

module AOC.Day7Bags (solutionPt1, solutionPt2) where

import AOC.Util
import Data.Attoparsec.ByteString.Char8
import Data.Map ((!))
import qualified Data.Map as M

type Color = String

type BagRule = (Int, Color)

type Regulations = M.Map Color [BagRule]

noBags :: Parser [BagRule]
noBags = do
  "no other bags."
  return []

singleBagRule :: Parser BagRule
singleBagRule = do
  num <- decimal
  space
  color <- manyTill anyChar (choice [string " bags", string " bag"])
  return (num, color)

bagRules :: Parser [BagRule]
bagRules = choice [noBags, singleBagRule `sepBy` ", " <* "."]

ruleParser :: Parser (Color, [BagRule])
ruleParser = do
  color <- manyTill anyChar " bags contain "
  rules <- bagRules
  return (color, rules)

parser :: Parser Regulations
parser = do
  rules <- ruleParser `sepBy` endOfLine
  return $ M.fromList rules

colorCanContain :: Regulations -> Color -> Color -> Bool
colorCanContain regs c target
  | c == target = False
  | otherwise = tryColors (regs ! c) target
  where
    tryColors :: [BagRule] -> Color -> Bool
    tryColors [] _ = False
    tryColors (tc : cs) t
      | snd tc == t = True
      | otherwise = tryColors cs t || colorCanContain regs (snd tc) t

countColor :: Color -> Regulations -> Int
countColor target regs =
  let matches = M.mapWithKey (\color _ -> colorCanContain regs color target) regs
      (total, _) = M.mapAccum (\acc b -> (acc + if b then 1 else 0, b)) 0 matches
   in total

countRequiredBags :: Color -> Regulations -> Int
countRequiredBags target regs = countRules (regs ! target)
  where
    countRules [] = 0
    countRules ((num, color) : rs) = num + num * countRequiredBags color regs + countRules rs

solutionPt1 :: String -> IO ()
solutionPt1 s = withParsedInput s parser $ countColor "shiny gold"

solutionPt2 :: String -> IO ()
solutionPt2 s = withParsedInput s parser $ countRequiredBags "shiny gold"
