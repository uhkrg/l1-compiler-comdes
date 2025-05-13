module Compile.Liveness (calcLiveness) where

import Data.Array
import qualified Data.Map as Map
import qualified Data.Set as Set
import Compile.IR
import Compile.LocalPredicates (LocalPredicates(..), readLocal)

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
