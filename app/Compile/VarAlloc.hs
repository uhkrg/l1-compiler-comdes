module Compile.VarAlloc (allocVars, Location(..), LocationMapping) where

import Compile.IR
import Compile.LocalPredicates
import Compile.Liveness
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph
import Data.Array
import Data.List ((\\))
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.PSQueue as PQ

data Location = Register String | Memory Integer deriving Show

type LocationMapping = Map.Map Integer Location

allocVars :: IR -> LocationMapping
allocVars ir = addUnlive $ graphColoringToLocations convertToVarIdx $ greedyColoring graph $ simplicialEliminationOrdering graph
    where
        liveness = calcLiveness ir
        (graph, nodeFromVertex, _vertexFromNode) = graphFromEdges $ livenessToEdges liveness
        convertToVarIdx = (\(a,_,_) -> a) . nodeFromVertex
        addUnlive = flip Map.union $ Map.fromList [(v, Memory 4) | v <- Map.elems $ def $ readLocal ir]

livenessToEdges :: Array Integer (Set.Set Integer) -> [(Integer, Integer, [Integer])]
livenessToEdges = map (\(x, y) -> (x, x, Set.elems y)) . Map.assocs . foldl (Map.unionWith Set.union) Map.empty . fmap (\set -> Map.fromSet (`Set.delete` set) set)

simplicialEliminationOrdering :: Graph -> [Vertex]
simplicialEliminationOrdering g = genOrder $ initialPQ g
    where
        genOrder :: PQ.PSQ Vertex Int -> [Vertex]
        genOrder pq = if PQ.null pq then [] else minEl : genOrder minRemoved
            where
                minEl = PQ.key $ fromJust $ PQ.findMin pq
                minRemoved = foldl (flip $ PQ.adjust (subtract 1)) (PQ.deleteMin pq) (g ! minEl)
        initialPQ = foldl (flip $ uncurry PQ.insert) PQ.empty . map (flip (,) 0) . vertices

greedyColoring :: Graph -> [Vertex] -> Map.Map Vertex Int
greedyColoring g = foldl (\c v -> Map.insert v (nextColor v c) c) Map.empty
    where
        nextColor v c = head $ [1..length (g ! v) + 1] \\ mapMaybe (c Map.!?) (g ! v)

graphColoringToLocations :: (Vertex -> Integer) -> Map.Map Vertex Int -> LocationMapping
graphColoringToLocations convertVertex = foldl (\r (v, c) -> Map.insert (convertVertex v) (colorToLocation c) r) Map.empty . Map.assocs
    where
        colorToLocation 1  = Register "%ecx"
        colorToLocation 2  = Register "%esi"
        colorToLocation 3  = Register "%edi"
        colorToLocation 4  = Register "%r8d"
        colorToLocation 5  = Register "%r9d"
        colorToLocation 6  = Register "%r10d"
        colorToLocation 7  = Register "%r11d"
        colorToLocation 8  = Register "%r12d"
        colorToLocation 9  = Register "%r13d"
        colorToLocation 10 = Register "%r14d"
        colorToLocation 11 = Register "%r15d"
        colorToLocation n = Memory $ (*4) . subtract 10 $ toInteger n

