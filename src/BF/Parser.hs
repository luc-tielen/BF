{-#  LANGUAGE GADTs #-}

module BF.Parser ( parse ) where

import Data.Void
import Data.Text (Text)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (empty)
import BF.Types

type ParseErr = Void
type Parser = P.Parsec ParseErr Text

incrementPtr :: Parser Instruction
incrementPtr = P.char '>' >> return IncrementPtr

decrementPtr :: Parser Instruction
decrementPtr = P.char '<' >> return DecrementPtr

incrementValue :: Parser Instruction
incrementValue = P.char '+' >> return IncrementValue

decrementValue :: Parser Instruction
decrementValue = P.char '-' >> return DecrementValue

writeOutput :: Parser Instruction
writeOutput = P.char '.' >> return WriteOutput

readInput :: Parser Instruction
readInput = P.char ',' >> return ReadInput

loop :: Parser Instruction
loop = do
  P.char '['
  instructions <- P.many instruction
  P.char ']'
  return $ Loop instructions

instruction :: Parser Instruction
instruction =  incrementPtr
           P.<|> decrementPtr
           P.<|> incrementValue
           P.<|> decrementValue
           P.<|> writeOutput
           P.<|> readInput
           P.<|> loop

parse :: Parser [Instruction]
parse = P.many instruction

