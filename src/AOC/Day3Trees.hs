module AOC.Day3Trees (solutionPt1, solutionPt2) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.List.NonEmpty (fromList)
import Data.Semigroup

data Cell
  = OpenSpace
  | Tree
  deriving (Show, Eq)

parseCell :: Char -> Cell
parseCell '.' = OpenSpace
parseCell '#' = Tree

cellScore :: Cell -> Int
cellScore OpenSpace = 0
cellScore Tree = 1

type Row = [Cell]

type Grid = [Row]

parseGrid :: String -> Grid
parseGrid = fmap (cycle . fmap parseCell) . lines

data Env = Env
  { step :: (Int, Int),
    grid :: Grid
  }
  deriving (Show)

type Coordinates = (Int, Int)

data TraversalState = TraversalState
  { treeCount :: Int,
    coordinates :: Coordinates
  }
  deriving (Show)

type Traversal = ReaderT Env (State TraversalState) Int

solution :: Traversal
solution = do
  Env (stepX, stepY) g <- ask
  TraversalState count (x, y) <- lift get
  if y >= length g
    then return count
    else do
      lift . put $ TraversalState (count + cellScore (g !! y !! x)) (x + stepX, y + stepY)
      solution

solutionPt1 :: String -> IO ()
solutionPt1 s =
  let grid = parseGrid s
      numTrees = evalState (runReaderT solution (Env (3, 1) grid)) $ TraversalState 0 (0, 0)
   in print numTrees

solutionPt2 :: String -> IO ()
solutionPt2 s =
  let grid = parseGrid s
      envs = [Env (1, 1) grid, Env (3, 1) grid, Env (5, 1) grid, Env (7, 1) grid, Env (1, 2) grid]
      states = runReaderT solution <$> envs
      numTrees = flip evalState (TraversalState 0 (0, 0)) <$> states
   in print . getProduct . sconcat $ Product <$> fromList numTrees
