module AOC.Day6Customs (solutionPt1, solutionPt2) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S

type Group = [String]

groupAnswers :: Group -> Int
groupAnswers g = go g S.empty
  where
    go [] s = S.size s
    go (i : is) s = go is (s <> S.fromList i)

groupAnswers2 :: Group -> Int
groupAnswers2 g = go (tail g) $ S.fromList (head g)
  where
    go [] s = S.size s
    go (i : is) s = go is (S.intersection s (S.fromList i))

parseGroup :: Parser Group
parseGroup = many1 $ do
  indiv <- many1 letter_ascii
  eitherP endOfLine endOfInput
  return indiv

solutionPt1 :: String -> IO ()
solutionPt1 s = case parseOnly (parseGroup `sepBy` endOfLine) $ BS.pack s of
  Left err -> putStrLn err
  Right groups -> print $ sum $ groupAnswers <$> groups

solutionPt2 :: String -> IO ()
solutionPt2 s = case parseOnly (parseGroup `sepBy` endOfLine) $ BS.pack s of
  Left err -> putStrLn err
  Right groups -> print $ sum $ groupAnswers2 <$> groups
