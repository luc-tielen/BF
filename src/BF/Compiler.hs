{-# LANGUAGE OverloadedStrings, LambdaCase, RecursiveDo #-}

module BF.Compiler ( compile ) where

import Control.Monad.Reader hiding (void)
import BF.Types
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.AST.IntegerPredicate
import LLVM.AST.Type
import LLVM.AST.Operand hiding (File)
import LLVM.IRBuilder.Constant
import LLVM.Context
import LLVM.Module
import LLVM.Target
import LLVM.CodeGenOpt
import LLVM.Relocation as R
import LLVM.CodeModel as C
import Data.Foldable


data Externals
  = Externals
  { extPutChar :: Operand
  , extGetChar :: Operand
  }

data CompilerState
  = CompilerState
  { memory :: Operand
  , index :: Operand
  , externals :: Externals
  }

type CodegenM = ReaderT CompilerState (IRBuilderT ModuleBuilder)

compile :: [Instruction] -> IO ()
compile prog = do
  let mod = buildModule "bf" $ compileModule prog

  withContext $ \ctx -> do
    withModuleFromAST ctx mod $ \mod' -> do
      let opt = None
      withHostTargetMachine R.PIC C.Default opt $ \tm -> do
        writeLLVMAssemblyToFile (File "bf.ll") mod'
        writeObjectToFile tm (File "bf.o") mod'
        -- NOTE: still requires linking after this step (e.g.: clang -o bf bf.o)

compileModule :: [Instruction] -> ModuleBuilder ()
compileModule prog = do
  malloc <- extern "malloc" [i32] (ptr i8)
  free <- extern "free" [ptr i8] void
  memset <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  putchar <- extern "putchar" [i32] i32
  getchar <- extern "getchar" [] i32
  let exts = Externals putchar getchar

  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[argc, argv] -> mdo
    let byteCount = 30000 * 4
    memory <- call malloc [(int32 byteCount, [])]
    call memset [(memory, []), (int8 0, []), (int64 byteCount, []), (bit 0, [])]
    array <- memory `bitcast` ptr i32
    idx <- allocate i32 (int32 0)

    runReaderT (compileInstructions prog) $ CompilerState array idx exts

    call free [(memory, [])]
    ret (int32 0)

  pure ()

-- TODO: refactor even more
compileInstructions :: [Instruction] -> CodegenM ()
compileInstructions = traverse_ compileInstruction where
  compileInstruction = \case
    Loop insts -> do
      let isNonZero = do
            value <- readArray
            icmp NE value (int32 0)

      whileLoop isNonZero $
        compileInstructions insts
    IncrementPtr -> modifyIndex increment
    DecrementPtr -> modifyIndex decrement
    IncrementValue -> modifyArray increment
    DecrementValue -> modifyArray decrement
    ReadInput -> do
      getChar <- asks (extGetChar . externals)
      char <- call getChar []
      modifyArray (const $ pure char)
    WriteOutput -> do
      putChar <- asks (extPutChar . externals)
      value <- readArray
      call putChar [(value, [])]
      pure ()
  increment = add (int32 1)
  decrement = (`sub` int32 1)

modifyIndex :: (Operand -> CodegenM Operand) -> CodegenM ()
modifyIndex f = do
  idxPtr <- asks index
  idx <- load idxPtr 0  -- TODO use load'
  idx' <- f idx
  store idxPtr 0 idx'  -- TODO use store'

modifyArray :: (Operand -> CodegenM Operand) -> CodegenM ()
modifyArray f = do
  CompilerState array idxPtr _ <- ask
  idx <- load idxPtr 0  -- TODO use load'
  valuePtr <- gep array [idx]
  value <- load valuePtr 0
  value' <- f value
  store valuePtr 0 value'  -- TODO use store'

readArray :: CodegenM Operand
readArray = do
  CompilerState array idxPtr externals <- ask
  idx <- load idxPtr 0  -- TODO use load'
  valuePtr <- gep array [idx]
  load valuePtr 0  -- TODO use load'

writeArray :: Operand -> CodegenM ()
writeArray value = modifyArray (const $ pure value)

-- TODO: update combinator

allocate :: Type -> Operand -> IRBuilderT ModuleBuilder Operand
allocate ty value = do
  addr <- alloca ty (Just (int32 1)) 0
  store' addr value
  pure addr

load' :: Operand -> IRBuilderT ModuleBuilder Operand
load' addr = load addr 0

store' :: Operand -> Operand -> IRBuilderT ModuleBuilder ()
store' addr value = store addr 0 value

whileLoop :: CodegenM Operand -> CodegenM a -> CodegenM ()
whileLoop condition asm = mdo
  br begin

  begin <- block `named` "while.begin"
  result <- condition
  condBr result body end

  body <- block `named` "while.body"
  asm
  br begin

  end <- block `named` "while.end"
  pure ()

