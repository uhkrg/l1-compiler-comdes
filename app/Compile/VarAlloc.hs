module Compile.VarAlloc (replaceVars) where

import Compile.Instr (Argument (..), Arguments (..), Instruction (..), Register (..))

replaceVars :: [Instruction] -> [Instruction]
replaceVars = concatMap replaceVarsInst

replaceVarsInst :: Instruction -> [Instruction]
replaceVarsInst (Instruction inst (SingleArgument (ArgumentVariable var))) = [readFstArg var, Instruction inst (SingleArgument $ ArgumentRegister RegEBX), writeFstArg var]
replaceVarsInst (Instruction inst (Arguments (ArgumentVariable var1) (ArgumentVariable var2))) = [readFstArg var1, readSndArg var2, Instruction inst (Arguments (ArgumentRegister RegEBX) (ArgumentRegister RegECX)), writeFstArg var1, writeSndArg var2]
replaceVarsInst (Instruction inst (Arguments (ArgumentVariable var1) arg2)) = [readFstArg var1, Instruction inst (Arguments (ArgumentRegister RegEBX) arg2), writeFstArg var1]
replaceVarsInst (Instruction inst (Arguments arg1 (ArgumentVariable var2))) = [readSndArg var2, Instruction inst (Arguments arg1 (ArgumentRegister RegECX)), writeSndArg var2]
replaceVarsInst inst = [inst]

readArg :: Register -> Integer -> Instruction
readArg reg var = Instruction "MOV" $ Arguments (ArgumentMemory var) (ArgumentRegister reg)

writeArg :: Register -> Integer -> Instruction
writeArg reg var = Instruction "MOV" $ Arguments (ArgumentRegister reg) (ArgumentMemory var)

readFstArg :: Integer -> Instruction
readFstArg = readArg RegEBX

writeFstArg :: Integer -> Instruction
writeFstArg = writeArg RegEBX

readSndArg :: Integer -> Instruction
readSndArg = readArg RegECX

writeSndArg :: Integer -> Instruction
writeSndArg = writeArg RegECX
