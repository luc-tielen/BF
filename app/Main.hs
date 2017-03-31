
module Main where

import Lib
import qualified Text.Megaparsec as P
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  handleArgs args

handleArgs :: [String] -> IO ()
handleArgs (filename:_) = do
  programString <- readFile filename
  case P.parse parse filename programString of
    Left e -> print e
    Right instructions -> interpret instructions
handleArgs _ = do
  progName <- getProgName
  print $ "Usage: " ++ progName ++ " FILENAME"
