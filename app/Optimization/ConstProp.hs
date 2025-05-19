module Optimization.ConstProp (constantPropagate) where

import qualified Data.Map as Map
import qualified Data.Array as Array

import Data.Int (Int32)

import Compile.LocalPredicates
import Compile.IR

constantPropagate :: IR -> IR
constantPropagate ir = ir Array.// prop Map.empty 1
    where
        succs = suc $ readLocal ir
        prop :: Map.Map Integer Integer -> Integer -> [(Integer, IRStatement)]
        prop replacements line = (line, stmt) : concatMap (prop newReplacements) (succs Array.! line)
            where
                stmt = constantFold $ fillConstant replacements $ ir Array.! line
                newReplacements = Map.union replacements $ readConstant stmt


readConstant :: IRStatement -> Map.Map Integer Integer
readConstant (Var v :|<-: Constant c) = Map.singleton v c
readConstant _ = Map.empty


fillConstant :: Map.Map Integer Integer -> IRStatement -> IRStatement
fillConstant replacements (Ret arg) = Ret $ replaceIfExists replacements arg
fillConstant replacements (v :|<-: val) = v :|<-: replaceIfExists replacements val
fillConstant replacements (v :<-: (val1, op, val2)) = v :<-: (replaceIfExists replacements val1, op, replaceIfExists replacements val2)
fillConstant replacements (v :<-^: (op, val)) = v :<-^: (op, replaceIfExists replacements val)

replaceIfExists :: Map.Map Integer Integer -> ValueContainer -> ValueContainer
replaceIfExists _ c@(Constant _) = c
replaceIfExists replacements var@(Variable (Var v))
    | v `Map.member` replacements = Constant $ replacements Map.! v
    | otherwise = var


constantFold :: IRStatement -> IRStatement
constantFold stmt@(_ :<-: (_, Div, Constant 0)) = stmt
constantFold stmt@(_ :<-: (_, Mod, Constant 0)) = stmt
constantFold stmt@(_ :<-: (Constant (-2147483648), Div, Constant (-1))) = stmt
constantFold stmt@(_ :<-: (Constant (-2147483648), Mod, Constant (-1))) = stmt
constantFold stmt@(_ :<-: (Constant (2147483648), Div, Constant (-1))) = stmt
constantFold stmt@(_ :<-: (Constant (2147483648), Mod, Constant (-1))) = stmt
constantFold (v :<-: (Constant 0, Div, _)) = v :|<-: Constant 0
constantFold (v :<-: (Constant 0, Mod, _)) = v :|<-: Constant 0

constantFold (v :<-: (Constant 0, Mul, _)) = v :|<-: Constant 0
constantFold (v :<-: (_, Mul, Constant 0)) = v :|<-: Constant 0
constantFold (v :<-: (Constant 1, Mul, other)) = v :|<-: other
constantFold (v :<-: (other, Mul, Constant 1)) = v :|<-: other

constantFold (v :<-: (Constant 0, Add, other)) = v :|<-: other
constantFold (v :<-: (other, Add, Constant 0)) = v :|<-: other

constantFold (v :<-: (Constant 0, Sub, v2@(Variable _))) = v :<-^: (Neg, v2)
constantFold (v :<-: (Constant 0, Sub, Constant c)) = v :|<-: Constant (-c)
constantFold (v :<-: (other, Sub, Constant 0)) = v :|<-: other

constantFold (v :<-: (Constant c1, op, Constant c2)) = v :|<-: Constant (keepInBounds $ apply op c1 c2)

constantFold (v :<-^: (Neg, Constant c)) = v :|<-: Constant (-c)

constantFold stmt = stmt


keepInBounds :: Integer -> Integer
keepInBounds v = toInteger (fromInteger v :: Int32)


apply :: Op -> Integer -> Integer -> Integer
apply Mul = (*)
apply Div = quot
apply Mod = rem
apply Add = (+)
apply Sub = (-)

