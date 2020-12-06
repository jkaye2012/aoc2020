{-# LANGUAGE OverloadedStrings #-}

module AOC.Day5Seats (solutionPt1, solutionPt2) where

import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Data.List (find, sort)
import Data.Maybe (fromJust)

data RowPartition
  = F
  | B
  deriving (Show)

rowFromChar :: Char -> RowPartition
rowFromChar 'F' = F
rowFromChar 'B' = B

data SeatPartition
  = R
  | L
  deriving (Show)

seatFromChar :: Char -> SeatPartition
seatFromChar 'L' = L
seatFromChar 'R' = R

type SeatSpec = ([RowPartition], [SeatPartition])

type Seat = (Int, Int) -- (Row, Seat)

type Range = (Int, Int) -- Min, Max

type SeatConfiguration = (Range, Range) -- Rows, Seats

findRow :: Range -> [RowPartition] -> Int
findRow (min, _) [F] = min
findRow (_, max) [B] = max
findRow (min, max) (F : rs) = findRow (min, (min + max) `div` 2) rs
findRow (min, max) (B : rs) = findRow ((1 + max + min) `div` 2, max) rs

findSeat' :: Range -> [SeatPartition] -> Int
findSeat' (min, _) [L] = min
findSeat' (_, max) [R] = max
findSeat' (min, max) (L : rs) = findSeat' (min, (min + max) `div` 2) rs
findSeat' (min, max) (R : rs) = findSeat' ((1 + max + min) `div` 2, max) rs

findSeat :: SeatConfiguration -> SeatSpec -> Seat
findSeat (rows, seats) (rowPart, seatPart) = (findRow rows rowPart, findSeat' seats seatPart)

seatId :: Seat -> Int
seatId (row, seat) = row * 8 + seat

parser :: Parser SeatSpec
parser = do
  rows <- P.takeWhile1 $ inClass "FB"
  seats <- P.takeWhile1 $ inClass "LR"
  return (rowFromChar <$> BS.unpack rows, seatFromChar <$> BS.unpack seats)

solutionPt1 :: String -> IO ()
solutionPt1 s =
  case parseOnly (parser `sepBy` endOfLine) $ BS.pack s of
    Left err -> putStrLn err
    Right spec -> print . maximum $ seatId . findSeat ((0, 127), (0, 7)) <$> spec

solutionPt2 :: String -> IO ()
solutionPt2 s =
  case parseOnly (parser `sepBy` endOfLine) $ BS.pack s of
    Left err -> putStrLn err
    Right spec -> print . mySeat $ seatId <$> filter (\(r, _) -> r > 0 && r < 127) (findSeat ((0, 127), (0, 7)) <$> spec)
      where
        mySeat seats =
          let sorted = sort seats
              pairs = zip sorted (tail sorted)
              (before, _) = fromJust $ find (\(a, b) -> a + 2 == b) pairs
           in before + 1

-- >>> findSeat ((0, 127), (0, 7)) ([F, B, F, B, B, F, F], [R, L, R])
-- (44,5)
