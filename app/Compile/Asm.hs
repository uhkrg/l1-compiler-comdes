module Compile.Asm (genAsm, starterAsm) where

import Compile.Instr (Argument (..), Arguments (..), Instruction (..), Register (..))

starterAsm :: [String]
starterAsm = [".global main", ".global _main", ".text", "main:", "call _main", "mov %rax, %rdi", "mov $0x3C, %rax", "syscall", "_main:"]

genAsm :: [Instruction] -> [String]
genAsm = (starterAsm ++) . map genInst

genInst :: Instruction -> String
genInst (Instruction inst EmptyArgument) = inst
genInst (Instruction inst (SingleArgument arg)) = inst ++ ' ' : genArg arg
genInst (Instruction inst (Arguments arg1 arg2)) = inst ++ ' ' : genArg arg1 ++ ',' : genArg arg2

genArg :: Argument -> String
genArg (ArgumentConstant c) = '$' : c
genArg (ArgumentMemory var) = '-' : show (var * 4 + 4) ++ "(%rsp)"
genArg (ArgumentRegister reg) = genReg reg
genArg (ArgumentVariable var) = error $ "Unresolved variable " ++ show var ++ "in assembly gen"

genReg :: Register -> String
genReg RegEAX = "%eax"
genReg RegEDX = "%edx"
genReg RegEBX = "%ebx"
genReg RegECX = "%ecx"
genReg x = error $ "Unimplemented reg" ++ show x
