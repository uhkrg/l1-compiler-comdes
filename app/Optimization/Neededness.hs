module Optimization.Neededness (calcNeededness) where

import Data.Maybe (fromMaybe)
import Data.Array
import qualified Data.Map as Map
import qualified Data.Set as Set
import Compile.IR
import Compile.LocalPredicates (LocalPredicates(..), readLocal)

calcNeededness :: IR -> Array Integer (Set.Set Integer)
calcNeededness = calcNeedednessFromPredicates . readLocal

calcNeedednessFromPredicates :: LocalPredicates -> Array Integer (Set.Set Integer)
calcNeedednessFromPredicates preds = snd $ propagateNeededness bootstrapedNeededness
    where
        uses = use preds
        succs = suc preds
        necs = nec preds
        defs = def preds
        setNecs = fmap Set.fromList necs
        len = snd $ bounds uses
        bootstrapedNeededness = listArray (1,len) [fromMaybe Set.empty $ setNecs Map.!? i | i<-[1..len]]
        propagateNeededness neededness = foldl combine (False, neededness) [len,len-1..1]
        combine (changed, neededness) idx = (changed || not (Set.null newNeededness), neededness // [(idx, Set.union oldNeededness newNeededness)])
            where 
                oldNeededness = neededness ! idx
                newNeededness = Set.difference inheritedNeededness oldNeededness
                inheritedNeededness = Set.union directlyInheritedNeededness indirectlyInheritedNeededness
                directlyInheritedNeededness = delete (defs Map.!? idx) $ Set.unions $ map (neededness!) $ succs ! idx
                delete Nothing = id
                delete (Just v) = Set.delete v
                indirectlyInheritedNeededness = 
                    if 
                        fromMaybe False $ (defs Map.!? idx) >>= Just . flip Set.member (Set.unions $ map (neededness!) $ succs ! idx)
                    then
                        Set.fromList (uses ! idx)
                    else
                        Set.empty

