module Compile.Asm (genAsm) where

import Compile.IR
import Data.Array (elems)

starterAsm :: [String]
starterAsm = [".global main", ".global _main", ".text", "main:", "call _main", "mov %rax, %rdi", "mov $0x3C, %rax", "syscall", "_main:"]

genAsm :: IR -> [String]
genAsm = (starterAsm ++) . map genStmt . elems

genStmt :: IRStatement -> String
genStmt (Ret val) = unlines 
    [ "mov " ++ genVal val ++ ", %eax"
    , "ret"
    ]
genStmt (v :|<-: (Variable var)) = unlines
    [ "mov " ++  genVar var ++ ", %eax"
    , "mov " ++ "%eax, " ++ genVar v
    ]
genStmt (v :|<-: val) = "movl " ++ genVal val ++ ", " ++ genVar v
genStmt (v :<-: (val1, Div, val2)) = unlines
    [ "mov " ++ genVal val1 ++ ", %eax"
    , "mov " ++ genVal val2 ++ ", %ebx"
    , "cdq"
    , "idiv %ebx"
    , "mov %eax, " ++ genVar v
    ]
genStmt (v :<-: (val1, Mod, val2)) = unlines
    [ "mov " ++ genVal val1 ++ ", %eax"
    , "mov " ++ genVal val2 ++ ", %ebx"
    , "cdq"
    , "idiv %ebx"
    , "mov %edx, " ++ genVar v
    ]
genStmt (v :<-: (val1, op, val2)) = unlines 
    [ "mov " ++ genVal val1 ++ ", %eax"
    , genOp op ++ ' ' : genVal val2 ++ ", %eax"
    , "mov " ++ "%eax, " ++ genVar v
    ]
genStmt (v :<-^: (Neg, val)) = unlines
    [ "movl " ++ genVal val ++ ", " ++  genVar v
    , "negl " ++ genVar v
    ]

genVar :: Var -> String
genVar (Var v) = show (negate (v * 4 + 4)) ++ "(%rsp)"

genVal :: ValueContainer -> String
genVal (Variable v) = genVar v
genVal (Constant c) = '$' : show c

genOp :: Op -> String
genOp Add = "add"
genOp Sub = "sub"
genOp Mul = "imul"
genOp op = "unsupported bin op " ++ show op
