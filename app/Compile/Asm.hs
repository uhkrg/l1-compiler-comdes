module Compile.Asm (genAsm) where

import Compile.IR
import Compile.VarAlloc(LocationMapping, Location(..))
import Data.Array (elems)
import Data.Map ((!))

starterAsm :: [String]
starterAsm = [".global _start", ".global main", ".text", "_start:", "call main", "mov %rax, %rdi", "mov $0x3C, %rax", "syscall", "main:"]

genAsm :: LocationMapping -> IR -> [String]
genAsm locs = (starterAsm ++) . map (genStmt locs) . elems

genStmt :: LocationMapping -> IRStatement -> String
genStmt locs (Ret val) = unlines 
    [ "mov " ++ genVal locs val ++ ", %eax"
    , "ret"
    ]
genStmt locs (v :|<-: (Variable var)) = 
    if 
        isReg locs var || isReg locs v 
    then 
        if 
            genVar locs var /= genVar locs v
        then
            "mov " ++ genVar locs var ++ ", " ++ genVar locs v  ++ "\n"
        else
            ""
    else 
        unlines
        [ "mov " ++  genVar locs var ++ ", %eax"
        , "mov " ++ "%eax, " ++ genVar locs v
        ]
genStmt locs (v :|<-: val) = "movl " ++ genVal locs val ++ ", " ++ genVar locs v ++ "\n"
genStmt locs (v :<-: (val1, Div, val2)) = 
    if 
        isRegVar locs val2
    then
        unlines 
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "cdq"
        , "idiv " ++ genVal locs val2
        , "mov %eax, " ++ genVar locs v
        ]
    else
        unlines
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "mov " ++ genVal locs val2 ++ ", %ebx"
        , "cdq"
        , "idiv %ebx"
        , "mov %eax, " ++ genVar locs v
        ]
genStmt locs (v :<-: (val1, Mod, val2)) = 
    if 
        isRegVar locs val2
    then
        unlines 
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "cdq"
        , "idiv " ++ genVal locs val2
        , "mov %edx, " ++ genVar locs v
        ]
    else
        unlines
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "mov " ++ genVal locs val2 ++ ", %ebx"
        , "cdq"
        , "idiv %ebx"
        , "mov %edx, " ++ genVar locs v
        ]
genStmt locs (v :<-: (val1, Sub, val2)) = 
    if
        (isReg locs v || (isRegVar locs val1 && isRegVar locs val2)) && (genVar locs v /= genVal locs val2)
    then
        if 
            genVar locs v == genVal locs val1
        then
            "subl " ++ genVal locs val2 ++ ", " ++ genVar locs v ++ "\n"
        else
            unlines
            [ "mov " ++ genVal locs val1 ++ ", " ++ genVar locs v
            , "sub " ++ genVal locs val2 ++ ", " ++ genVar locs v
            ]
    else
        unlines 
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "sub " ++ genVal locs val2 ++ ", %eax"
        , "mov " ++ "%eax, " ++ genVar locs v
        ]
genStmt locs (v :<-: (val1, Add, val2)) = 
    if
        isReg locs v || (isRegVar locs val1 && isRegVar locs val2)
    then
        if 
            genVar locs v == genVal locs val2 
        then
            "add " ++ genVal locs val1 ++ ", " ++ genVar locs v ++ "\n"
        else if
            genVar locs v == genVal locs val1
        then
            "add " ++ genVal locs val2 ++ ", " ++ genVar locs v ++ "\n"
        else
            unlines
            [ "mov " ++ genVal locs val1 ++ ", " ++ genVar locs v
            , "add " ++ genVal locs val2 ++ ", " ++ genVar locs v
            ]
    else
        unlines 
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "add " ++ genVal locs val2 ++ ", %eax"
        , "mov " ++ "%eax, " ++ genVar locs v
        ]
genStmt locs (v :<-: (val1, Mul, val2)) = 
    if
        isReg locs v
    then
        if 
            genVar locs v == genVal locs val2 
        then
            "imul " ++ genVal locs val1 ++ ", " ++ genVar locs v ++ "\n"
        else if
            genVar locs v == genVal locs val1
        then
            "imul " ++ genVal locs val2 ++ ", " ++ genVar locs v ++ "\n"
        else
            unlines
            [ "mov " ++ genVal locs val1 ++ ", " ++ genVar locs v
            , "imul " ++ genVal locs val2 ++ ", " ++ genVar locs v
            ]
    else
        unlines 
        [ "mov " ++ genVal locs val1 ++ ", %eax"
        , "imul " ++ genVal locs val2 ++ ", %eax"
        , "mov " ++ "%eax, " ++ genVar locs v
        ]
genStmt locs (v :<-^: (Neg, val))
    | genVar locs v == genVal locs val = "negl " ++ genVar locs v ++ "\n"
    | isReg locs v || isRegVar locs val = unlines
        [ "mov " ++  genVal locs val ++ ", " ++ genVar locs v
        , "negl " ++ genVar locs v
        ]
    | otherwise = unlines
        [ "mov " ++ genVal locs val ++ ", %eax"
        , "neg %eax"
        , "mov %eax, " ++ genVar locs v
        ]

genVar :: LocationMapping -> Var -> String
genVar locs (Var v) = case locs ! v of 
    Register r -> r
    Memory m -> show (negate m) ++ "(%rsp)"

genVal :: LocationMapping -> ValueContainer -> String
genVal locs (Variable v) = genVar locs v
genVal _ (Constant c) = '$' : show c

isReg :: LocationMapping -> Var -> Bool
isReg locs (Var v) = case locs ! v of
    Register _ -> True
    _ -> False

isRegVar :: LocationMapping -> ValueContainer -> Bool
isRegVar locs (Variable v) = isReg locs v
isRegVar _ (Constant _) = False
