module AOC.Day11Seats (solutionPt1, solutionPt2) where

import AOC.Util
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import qualified Data.Map.Strict as M

data Seat
  = Floor
  | Empty
  | Occupied
  deriving (Eq, Show)

parseSeat :: Char -> Seat
parseSeat '.' = Floor
parseSeat 'L' = Empty
parseSeat '#' = Occupied

type Coord = (Int, Int)

type Grid = M.Map Coord Seat

indexed :: Int -> [Seat] -> [(Coord, Seat)]
indexed y s = go 0 y s
  where
    go _ _ [] = []
    go x y (s : ss) = [((y, x), s)] <> go (x + 1) y ss

parseGrid :: [[Seat]] -> Grid
parseGrid = go 0 M.empty
  where
    go :: Int -> Grid -> [[Seat]] -> Grid
    go _ g [] = g
    go y g (r : rs) = foldr (\(c, s) m -> M.insert c s m) g (indexed y r) <> go (y + 1) g rs

parser :: Parser ((Int, Int), Grid)
parser = do
  seats <- seatRow `sepBy` endOfLine
  let s = filter (not . null) seats
  return ((length . head $ s, length s), parseGrid s)
  where
    seatRow = do
      s <- many' $ choice [char '.', char 'L', char '#']
      return $ parseSeat <$> s

neighborCoords :: (Int, Int) -> Coord -> [Coord]
neighborCoords (maxX, maxY) (y, x) =
  [ (y', x') | x' <- (+ x) <$> [-1, 0, 1], y' <- (+ y) <$> [-1, 0, 1], (y', x') /= (y, x), x' >= 0 && x' < maxX, y' >= 0 && y' < maxY
  ]

neighbors :: (Int, Int) -> Grid -> Coord -> [Seat]
neighbors sz g c = (M.!) g <$> neighborCoords sz c

seatTransition :: Int -> Seat -> [Seat] -> Seat
seatTransition _ Empty ss
  | Occupied `notElem` ss = Occupied
seatTransition num Occupied ss
  | length (filter (== Occupied) ss) >= num = Empty
seatTransition _ s _ = s

type Rule = (Int, Int) -> Grid -> Coord -> Seat -> Seat

step :: Rule -> (Int, Int) -> Grid -> Grid
step r sz g = M.mapWithKey (r sz g) g

stabilize :: Rule -> (Int, Int) -> Grid -> Int
stabilize r sz g =
  if next == g
    then length $ M.filter (== Occupied) next
    else stabilize r sz next
  where
    next = step r sz g

testSeats :: String
testSeats =
  unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]

solutionPt1 :: String -> IO ()
solutionPt1 s = withParsedInput s parser $ uncurry $ stabilize (\sz g c s -> seatTransition 4 s $ neighbors sz g c)

neighbors' :: (Int, Int) -> Grid -> Coord -> [Seat]
neighbors' sz g c =
  let directions =
        [ (-1, -1),
          (-1, 0),
          (-1, 1),
          (0, -1),
          (0, 1),
          (1, -1),
          (1, 0),
          (1, 1)
        ]
   in findDirection sz g c <$> directions
  where
    next (dy, dx) c = bimap (+ dy) (+ dx) c
    findDirection sz g c d = findDirection' sz g (next d c) d
    findDirection' sz@(maxX, maxY) g c@(y, x) d
      | x < 0 || x >= maxX = Empty
      | y < 0 || y >= maxY = Empty
      | otherwise =
        case (M.!) g c of
          Floor -> findDirection' sz g (next d c) d
          other -> other

solutionPt2 :: String -> IO ()
solutionPt2 s = withParsedInput s parser $ uncurry $ stabilize (\sz g c s -> seatTransition 5 s $ neighbors' sz g c)
