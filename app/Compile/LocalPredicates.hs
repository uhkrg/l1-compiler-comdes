module Compile.LocalPredicates (LocalPredicates(..), readLocal) where

import Data.Array
import qualified Data.Map as Map
import Compile.IR

data LocalPredicates = LocalPredicates
    { def :: Map.Map Integer Integer -- line to variable
    , use :: Array Integer [Integer] -- line to variables
    , suc :: Array Integer [Integer] -- line to lines
    } deriving Show

readLocal :: IR -> LocalPredicates
readLocal = combine . transpose . map (uncurry readStmt) . assocs 
    where
        transpose [] = ([], [],[])
        transpose ((a, b, c): xs) = let (as, bs, cs) = transpose xs in (a:as, b:bs, c:cs)
        combine (defs, uses, succs) = LocalPredicates (Map.fromList $ concat defs) (toArray uses) (toArray succs)
        toArray lst = array (1, toInteger $ length lst) lst


readStmt :: Integer -> IRStatement -> ([(Integer, Integer)], (Integer, [Integer]), (Integer, [Integer]))
readStmt idx (Ret (Constant _))                                         = ([],         (idx, []),     (idx, [])       )
readStmt idx (Ret (Variable (Var v)))                                   = ([],         (idx, [v]),    (idx, [])       )

readStmt idx (Var x :|<-: Constant _)                                   = ([(idx, x)], (idx, []),     (idx, [idx + 1]))
readStmt idx (Var x :|<-: Variable (Var y))                             = ([(idx, x)], (idx, [y]),    (idx, [idx + 1]))

readStmt idx (Var x :<-^: (_op, Constant _))                            = ([(idx, x)], (idx, []),     (idx, [idx + 1]))
readStmt idx (Var x :<-^: (_op, Variable (Var y)))                      = ([(idx, x)], (idx, [y]),    (idx, [idx + 1]))

readStmt idx (Var x :<-: (Constant _, _op, Constant _))                 = ([(idx, x)], (idx, []),     (idx, [idx + 1]))
readStmt idx (Var x :<-: (Variable (Var y), _op, Constant _))           = ([(idx, x)], (idx, [y]),    (idx, [idx + 1]))
readStmt idx (Var x :<-: (Constant _, _op, Variable (Var y)))           = ([(idx, x)], (idx, [y]),    (idx, [idx + 1]))
readStmt idx (Var x :<-: (Variable (Var y), _op, Variable (Var z)))     = ([(idx, x)], (idx, [y, z]), (idx, [idx + 1]))
