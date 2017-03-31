
module BF.Interpreter ( interpret
                      ) where

import Control.Monad.State
import Data.Array.Unboxed hiding (array)
import qualified Data.Array.IArray as A
import Data.Word
import BF.Types

type Index = Int
type Byte = Word8
type ByteArray = UArray Index Byte
data Interpreter = Interpreter {
  idx :: Index,
  array :: ByteArray
}
type InterpreterStateT = StateT Interpreter IO ()


evalStateT' :: Monad m => s -> StateT s m a -> m a
evalStateT' = flip evalStateT

evalInstruction :: Instruction -> InterpreterStateT
evalInstruction IncrementPtr = modify $ \s@(Interpreter i _) -> s { idx = i + 1 }
evalInstruction DecrementPtr = modify $ \s@(Interpreter i _) -> s { idx = i - 1 }
evalInstruction IncrementValue = modify $ \s@(Interpreter i a) -> s { array = increment i a }
  where increment i a = a // [(i, (a ! i) + 1)]  --ixmap (i, i) (\v -> v + 1) a
evalInstruction DecrementValue = modify $ \s@(Interpreter i a) -> s { array = decrement i a }
  where decrement i a = a // [(i, (a ! i) - 1)]
evalInstruction WriteOutput = do
  Interpreter i a <- get
  let v = a ! i
  liftIO $ putChar . toEnum . fromIntegral $ v
evalInstruction ReadInput = do
  s@(Interpreter i a1) <- get
  input <- liftIO $ getChar
  let input' = fromIntegral . fromEnum $ input
  let a2 = a1 // [(i, input')]
  put s { array = a2 }
evalInstruction loop@(Loop instructions) = do
  Interpreter i a <- get
  let v = a ! i
  case v of
    0 -> return ()
    _ -> evalInstructions instructions >> evalInstruction loop

evalInstructions :: [Instruction] -> InterpreterStateT
evalInstructions = mapM_ evalInstruction

interpret :: [Instruction] -> IO ()
interpret instructions = do
  evalStateT' interpreter $ evalInstructions instructions
  where interpreter = Interpreter 0 makeArray
        makeArray = A.array (0, lenArray - 1) [(i, 0) | i <- [0 .. lenArray - 1]]
        lenArray = 30000
