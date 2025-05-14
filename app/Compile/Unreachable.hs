module Compile.Unreachable (removeUnreachable) where

import Data.Array (Array, (!), listArray)
import qualified Data.Set as Set

import Compile.IR (IR)
import Compile.LocalPredicates (LocalPredicates(suc), readLocal)

removeUnreachable :: IR -> IR
removeUnreachable ir = onlyReachable reachable ir
    where
        succs = suc $ readLocal ir
        reachable = genReachable succs [1]

genReachable :: Array Integer [Integer] -> [Integer] -> Set.Set Integer
genReachable _ [] = Set.empty
genReachable succs (todo:todos) = Set.insert todo $ Set.union (genReachable succs (succs ! todo)) (genReachable succs todos)

onlyReachable :: Set.Set Integer -> IR -> IR
onlyReachable reachable ir = listArray (1, toInteger $ Set.size reachable) [ir ! i | i <- Set.toAscList reachable]
