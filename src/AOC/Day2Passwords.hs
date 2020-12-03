{-# LANGUAGE OverloadedStrings #-}

module AOC.Day2Passwords (solutionPt1, solutionPt2) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

data PasswordEntry = PasswordEntry
  { requiredChar :: Char,
    minRequired :: Int,
    maxRequired :: Int,
    password :: String
  }
  deriving (Show)

passwordValid :: PasswordEntry -> Bool
passwordValid (PasswordEntry req min max pw) =
  let cnt = length $ filter (== req) pw
   in cnt >= min && cnt <= max

type Input = [PasswordEntry]

entryParser :: Parser PasswordEntry
entryParser = do
  min <- decimal
  char '-'
  max <- decimal
  char ' '
  req <- letter_ascii
  ": "
  pw <- many' letter_ascii
  skipMany endOfLine
  return $ PasswordEntry req min max pw

parser :: Parser Input
parser = do
  many1 entryParser

solutionPt1 :: String -> IO ()
solutionPt1 s =
  case parseOnly parser $ BS.pack s of
    Left err -> putStrLn err
    Right res -> print numValid
      where
        numValid = length $ filter passwordValid res

passwordValidPt2 :: PasswordEntry -> Bool
passwordValidPt2 (PasswordEntry req min max pw) =
  (pw !! (min - 1) == req) /= (pw !! (max - 1) == req)

solutionPt2 :: String -> IO ()
solutionPt2 s =
  case parseOnly parser $ BS.pack s of
    Left err -> putStrLn err
    Right res -> print numValid
      where
        numValid = length $ filter passwordValidPt2 res
