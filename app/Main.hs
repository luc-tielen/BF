
module Main where

import Lib
import qualified Text.Megaparsec as P


main :: IO ()
main = do
  let filename = "test.bf"
  programString <- readFile filename
  case P.parse parse filename programString of
    Left e -> print e
    Right instructions -> interpret instructions
