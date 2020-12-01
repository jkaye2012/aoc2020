{-# LANGUAGE OverloadedStrings #-}

module Download where

import AOC.Util
import Options.Applicative

newtype DownloadArgs = DownloadArgs
  { puzzleNumber :: Int
  }

args :: Parser DownloadArgs
args =
  DownloadArgs
    <$> argument auto (metavar "NUMBER")

main :: IO ()
main = do
  a <- execParser cmdline
  write (puzzleNumber a)
  where
    cmdline =
      info
        (args <**> helper)
        (fullDesc <> progDesc "Download the inputs for a specific AOC puzzle")
