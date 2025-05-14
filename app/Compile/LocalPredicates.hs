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
readDef idx (Ret (Constant _))                                         = []
readDef idx (Ret (Variable (Var v)))                                   = []

readDef idx (Var x :|<-: Constant _)                                   = [(idx, x)]
readDef idx (Var x :|<-: Variable (Var y))                             = [(idx, x)]

readDef idx (Var x :<-^: (_op, Constant _))                            = [(idx, x)]
readDef idx (Var x :<-^: (_op, Variable (Var y)))                      = [(idx, x)]

readDef idx (Var x :<-: (Constant _, _op, Constant _))                 = [(idx, x)]
readDef idx (Var x :<-: (Variable (Var y), _op, Constant _))           = [(idx, x)]
readDef idx (Var x :<-: (Constant _, _op, Variable (Var y)))           = [(idx, x)]
readDef idx (Var x :<-: (Variable (Var y), _op, Variable (Var z)))     = [(idx, x)]


readUses :: Integer -> IRStatement -> (Integer, [Integer])
readUses idx (Ret (Constant _))                                         = (idx, []) 
readUses idx (Ret (Variable (Var v)))                                   = (idx, [v])

readUses idx (Var x :|<-: Constant _)                                   = (idx, [])
readUses idx (Var x :|<-: Variable (Var y))                             = (idx, [y])

readUses idx (Var x :<-^: (_op, Constant _))                            = (idx, [])
readUses idx (Var x :<-^: (_op, Variable (Var y)))                      = (idx, [y])

readUses idx (Var x :<-: (Constant _, _op, Constant _))                 = (idx, [])
readUses idx (Var x :<-: (Variable (Var y), _op, Constant _))           = (idx, [y])
readUses idx (Var x :<-: (Constant _, _op, Variable (Var y)))           = (idx, [y])
readUses idx (Var x :<-: (Variable (Var y), _op, Variable (Var z)))     = (idx, [y, z])


readSuccs :: Integer -> IRStatement -> (Integer, [Integer])
readSuccs idx (Ret (Constant _))                                         = (idx, [])
readSuccs idx (Ret (Variable (Var v)))                                   = (idx, [])

readSuccs idx (Var x :|<-: Constant _)                                   = (idx, [idx + 1])
readSuccs idx (Var x :|<-: Variable (Var y))                             = (idx, [idx + 1])

readSuccs idx (Var x :<-^: (_op, Constant _))                            = (idx, [idx + 1])
readSuccs idx (Var x :<-^: (_op, Variable (Var y)))                      = (idx, [idx + 1])

readSuccs idx (Var x :<-: (Constant _, _op, Constant _))                 = (idx, [idx + 1])
readSuccs idx (Var x :<-: (Variable (Var y), _op, Constant _))           = (idx, [idx + 1])
readSuccs idx (Var x :<-: (Constant _, _op, Variable (Var y)))           = (idx, [idx + 1])
readSuccs idx (Var x :<-: (Variable (Var y), _op, Variable (Var z)))     = (idx, [idx + 1])

