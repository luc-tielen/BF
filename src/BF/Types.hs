
module BF.Types ( Instruction(..)
                ) where

data Instruction = IncrementPtr
                 | DecrementPtr
                 | IncrementValue
                 | DecrementValue
                 | WriteOutput
                 | ReadInput
                 | Loop [Instruction]
                 deriving (Eq, Show)
