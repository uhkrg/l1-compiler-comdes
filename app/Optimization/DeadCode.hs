module Optimization.DeadCode (removeDeadCode) where

import Data.Array
import qualified Data.Set as Set

import Compile.IR
import Compile.LocalPredicates
import Optimization.Neededness

removeDeadCode :: IR -> IR
removeDeadCode ir = array ((,) 1 . toInteger . length $ withoutDeadCode) withoutDeadCode
    where
        succs = suc $ readLocal ir
        neededs = calcNeededness ir
        withoutDeadCode = filter (uncurry $ isNotDead succs neededs) $ assocs ir



isNotDead :: Array Integer [Integer] -> Array Integer (Set.Set Integer) -> Integer -> IRStatement -> Bool
isNotDead _ _ _ (Ret _) = True
isNotDead succs neededs idx (Var x :|<-: _) = isNeededInSucc succs neededs idx x
isNotDead succs neededs idx (Var x :<-^: _) = isNeededInSucc succs neededs idx x
isNotDead succs neededs idx (Var x :<-: (_, op, _)) = inducesNecOp op || isNeededInSucc succs neededs idx x


isNeededInSucc :: Array Integer [Integer] -> Array Integer (Set.Set Integer) -> Integer -> Integer -> Bool
isNeededInSucc succs neededs idx var = Set.member var $ Set.unions $ map (neededs !) $ succs ! idx

