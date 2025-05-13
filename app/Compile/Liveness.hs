module Compile.Liveness (calcLiveness) where

import Data.Array
import qualified Data.Map as Map
import qualified Data.Set as Set
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


calcLiveness :: IR -> Array Integer (Set.Set Integer)
calcLiveness = calcLivenessFromPredicates . readLocal

calcLivenessFromPredicates :: LocalPredicates -> Array Integer (Set.Set Integer)
calcLivenessFromPredicates preds = snd $ propagateLiveness bootstrapedLiveness
    where
        uses = use preds
        succs = suc preds
        defs = def preds
        len = snd $ bounds uses
        bootstrapedLiveness = fmap Set.fromList uses
        propagateLiveness liveness = foldl combine (False, liveness) [len,len-1..1]
        combine (changed, liveness) idx = (changed || not (Set.null newLiveness), liveness // [(idx, Set.union oldLiveness newLiveness)])
            where 
                oldLiveness = liveness ! idx
                newLiveness = Set.difference inheritedLiveness oldLiveness
                inheritedLiveness = delete (defs Map.!? idx) $ Set.unions $ map (liveness!) $ succs ! idx
                delete Nothing = id
                delete (Just v) = Set.delete v
