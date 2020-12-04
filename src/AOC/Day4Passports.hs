{-# LANGUAGE OverloadedStrings #-}

module AOC.Day4Passports (solutionPt1, solutionPt2) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Text.Read (readMaybe)

data Passport = Passport
  { birthYear :: String,
    issueYear :: String,
    expirationYear :: String,
    height :: String,
    hairColor :: String,
    eyeColor :: String,
    passportId :: String,
    countryId :: Maybe String
  }

type PassportDbEntry = M.Map String String

passportDbParser :: Parser [PassportDbEntry]
passportDbParser = do
  kvps <- entry `sepBy` endOfLine
  return $ M.fromList <$> kvps
  where
    entry = many1 $ do
      key <- many1 letter_ascii
      char ':'
      val <- manyTill anyChar $ eitherP (char ' ') endOfLine
      return (key, val)

parseEntry :: PassportDbEntry -> Maybe Passport
parseEntry e = do
  byr <- M.lookup "byr" e
  iyr <- M.lookup "iyr" e
  eyr <- M.lookup "eyr" e
  hgt <- M.lookup "hgt" e
  hcl <- M.lookup "hcl" e
  ecl <- M.lookup "ecl" e
  pid <- M.lookup "pid" e
  let cid = M.lookup "cid" e
  return $ Passport byr iyr eyr hgt hcl ecl pid cid

solutionPt1 :: String -> IO ()
solutionPt1 s = case parseOnly passportDbParser $ BS.pack s of
  Left err -> putStrLn err
  Right entries -> do
    print . length . filter isJust . fmap parseEntry $ entries

data Unit = Cm | In

parseUnit :: BS.ByteString -> Unit
parseUnit "cm" = Cm
parseUnit "in" = In

validUnit :: Unit -> Int -> Bool
validUnit Cm num = num >= 150 && num <= 193
validUnit In num = num >= 59 && num <= 76

parseHeight :: Parser (Int, Unit)
parseHeight = do
  num <- decimal
  unit <- choice [string "cm", string "in"]
  return (num, parseUnit unit)

validateHeight :: String -> Bool
validateHeight hgt = case parseOnly parseHeight $ BS.pack hgt of
  Left _ -> False
  Right (num, unit) -> validUnit unit num

validateHairColor :: String -> Bool
validateHairColor = isRight . parseOnly parseHairColor . BS.pack
  where
    parseHairColor = do
      char '#'
      count 6 $ eitherP digit (satisfy $ inClass "a-f")

validateEyeColor :: String -> Bool
validateEyeColor "amb" = True
validateEyeColor "blu" = True
validateEyeColor "brn" = True
validateEyeColor "gry" = True
validateEyeColor "grn" = True
validateEyeColor "hzl" = True
validateEyeColor "oth" = True
validateEyeColor _ = False

validatePassport :: Passport -> Bool
validatePassport (Passport byr iyr eyr hgt hcl ecl pid _) =
  validateYear byr 1920 2002
    && validateYear iyr 2010 2020
    && validateYear eyr 2020 2030
    && validateHeight hgt
    && validateHairColor hcl
    && validateEyeColor ecl
    && length pid == 9
    && all isDigit pid
  where
    readInt i = readMaybe i :: Maybe Int

    isSatisfied Nothing = False
    isSatisfied (Just res) = res

    validateYear year min max =
      length year == 4
        && isSatisfied
          ( readInt year >>= \y -> return $ y >= min && y <= max
          )

solutionPt2 :: String -> IO ()
solutionPt2 s = case parseOnly passportDbParser $ BS.pack s of
  Left err -> putStrLn err
  Right entries -> do
    case sequenceA . filter isJust . fmap parseEntry $ entries of
      Just passports -> print . length . filter validatePassport $ passports
      Nothing -> putStrLn "Traverse failed"
