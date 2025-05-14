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
readLocal = calcPreds . assocs 
    where
        calcPreds asscs = LocalPredicates (calcDefs asscs) (calcUses asscs) (calcSucs asscs)
        calcDefs = toMap . map (uncurry readDef)
        calcUses = toArray . map (uncurry readUses)
        calcSucs = toArray . map (uncurry readSuccs)
        toMap = Map.fromList . concat
        toArray lst = array (1, toInteger $ length lst) lst


readDef :: Integer -> IRStatement -> [(Integer, Integer)]
readDef _ (Ret _)             = []
readDef idx (Var x :|<-: _)   = [(idx, x)]
readDef idx (Var x :<-^: _)   = [(idx, x)]
readDef idx (Var x :<-: _)    = [(idx, x)]


readUses :: Integer -> IRStatement -> (Integer, [Integer])
readUses idx (Ret (Constant _))                                         = (idx, []) 
readUses idx (Ret (Variable (Var v)))                                   = (idx, [v])

readUses idx (_ :|<-: Constant _)                                   = (idx, [])
readUses idx (_ :|<-: Variable (Var y))                             = (idx, [y])

readUses idx (_ :<-^: (_op, Constant _))                            = (idx, [])
readUses idx (_ :<-^: (_op, Variable (Var y)))                      = (idx, [y])

readUses idx (_ :<-: (Constant _, _op, Constant _))                 = (idx, [])
readUses idx (_ :<-: (Variable (Var y), _op, Constant _))           = (idx, [y])
readUses idx (_ :<-: (Constant _, _op, Variable (Var y)))           = (idx, [y])
readUses idx (_ :<-: (Variable (Var y), _op, Variable (Var z)))     = (idx, [y, z])


readSuccs :: Integer -> IRStatement -> (Integer, [Integer])
readSuccs idx (Ret _)       = (idx, [])
readSuccs idx (_:|<-: _)    = (idx, [idx + 1])
readSuccs idx (_ :<-^: _)   = (idx, [idx + 1])
readSuccs idx (_ :<-: _)    = (idx, [idx + 1])

