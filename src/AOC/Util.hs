{-# LANGUAGE OverloadedStrings #-}

module AOC.Util (session, inputDirectory, inputPath, withPuzzleInput, write) where

import Conduit (MonadThrow)
import Configuration.Dotenv
import Control.Exception
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<&>))
import Data.List (intercalate)
import Network.HTTP.Simple
import System.Directory (doesFileExist)
import System.Environment (getEnv, getEnvironment)
import System.FilePath

sessionVar :: String
sessionVar = "AOC_SESSION"

inputDirectoryVar :: String
inputDirectoryVar = "INPUT_DIR"

envComplete :: [String] -> Bool
envComplete args = sessionVar `elem` args && inputDirectoryVar `elem` args

envLoaded :: IO Bool
envLoaded = envComplete <$> (fmap fst <$> getEnvironment)

newtype MissingEnvVars = MissingEnvVars String
  deriving (Show)

instance Exception MissingEnvVars

ensureEnv :: IO ()
ensureEnv = do
  loaded <- envLoaded
  unless loaded $ do
    vars <- loadFile defaultConfig
    let args = fst <$> vars
    unless (envComplete args) $ do
      throw $ MissingEnvVars $ "Missing one or more environment variables: " <> intercalate "," [sessionVar, inputDirectoryVar]

getEnv' :: String -> IO String
getEnv' s = ensureEnv >> getEnv s

-- | Retrieves the AOC session key from the current environment.
--
-- This key is used for automated download of AOC puzzle inputs.
session :: IO String
session = getEnv' sessionVar

-- | Retrieves the directory where puzzle inputs should be cached from the current environment.
inputDirectory :: IO String
inputDirectory = getEnv' inputDirectoryVar

-- | Retrieves the local path to a specific puzzle input cache.
inputPath :: Int -> IO FilePath
inputPath num = inputDirectory <&> (</> (show num <.> ".txt"))

puzzleCached :: FilePath -> IO Bool
puzzleCached = doesFileExist

inputRequest :: MonadThrow m => Int -> String -> m Request
inputRequest num session = do
  parseRequest
    ("https://adventofcode.com/2020/day/" <> show num <> "/input")
    <&> setRequestHeader "Cookie" [BS.pack $ "session=" <> session]

download :: Int -> IO BS.ByteString
download num = do
  putStrLn $ "Downloading input for puzzle #" <> show num
  s <- session
  req <- inputRequest num s
  httpBS req <&> getResponseBody

-- | Download and write a puzzle's input to a local cache.
write :: Int -> IO ()
write num = do
  contents <- download num
  path <- inputPath num
  writeFile path $ BS.unpack contents

-- | Run a puzzle solver for the provided puzzle number.
withPuzzleInput :: Int -> (String -> IO ()) -> IO ()
withPuzzleInput num solution = do
  path <- inputPath num
  cached <- puzzleCached path
  unless cached (write num)
  readFile path >>= solution
