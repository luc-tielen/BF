
{-#  LANGUAGE GADTs #-}

module BF.Parser ( parse ) where


import Text.Megaparsec hiding (parse)
import Text.Megaparsec.String
import Text.Megaparsec.Prim hiding (parse)
import qualified Text.Megaparsec.Lexer as L
import Control.Applicative (empty)
import BF.Types


incrementPtr :: Parser Instruction
incrementPtr = char '>' >> return IncrementPtr

decrementPtr :: Parser Instruction
decrementPtr = char '<' >> return DecrementPtr

incrementValue :: Parser Instruction
incrementValue = char '+' >> return IncrementValue

decrementValue :: Parser Instruction
decrementValue = char '-' >> return DecrementValue

writeOutput :: Parser Instruction
writeOutput = char '.' >> return WriteOutput

readInput :: Parser Instruction
readInput = char ',' >> return ReadInput

loop :: Parser Instruction
loop = do
  char '['
  instructions <- many instruction
  char ']'
  return $ Loop instructions

instruction :: Parser Instruction
instruction =  incrementPtr
           <|> decrementPtr
           <|> incrementValue
           <|> decrementValue
           <|> writeOutput
           <|> readInput
           <|> loop

parse :: Parser [Instruction]
parse = many instruction

