
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
instruction =  lexeme
            $  incrementPtr
           <|> decrementPtr
           <|> incrementValue
           <|> decrementValue
           <|> writeOutput
           <|> readInput
           <|> loop

parse :: Parser [Instruction]
parse = many instruction


-- Helper functions:

-- Anything not parsed by above parser is considered 'whitespace'
whitespace :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => m ()
whitespace = L.space spaceParser commentParser blockCommentParser
  where spaceParser = noneOf "><+-.,[]" >> return ()
        commentParser = empty
        blockCommentParser = empty

lexeme :: (Token s ~ Char, Text.Megaparsec.Prim.MonadParsec e s m) => m a -> m a
lexeme = L.lexeme whitespace
