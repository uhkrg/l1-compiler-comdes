module Compile.Instr (Instruction (..), Argument (..), Arguments (..), Register (..)) where

data Instruction = Instruction
  { instr :: String,
    args :: Arguments
  }

data Arguments = EmptyArgument | SingleArgument Argument | Arguments {source :: Argument, target :: Argument}

data Argument = ArgumentVariable Integer | ArgumentRegister Register | ArgumentConstant String | ArgumentMemory Integer
  deriving (Show)

data Register = RegEAX | RegEBX | RegECX | RegEDX | RegESI | RegEDI | RegESP | RegEBP | RegR8D | RegR9D | RegR10D | RegR11D | RegR12D | RegR13D | RegR14D | RegR15D
  deriving (Show)

instance Show Instruction where
  show (Instruction instr args) = instr ++ show args

instance Show Arguments where
  show EmptyArgument = ""
  show (SingleArgument arg) = ' ' : show arg
  show (Arguments source target) = ' ' : show source ++ ',' : show target
