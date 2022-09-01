module SP where

import Prelude

import Data.List (List(..), singleton, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Graph (class Graph, Context, LPath(..), Node, emptyLP, isEmpty, match)
import Heap as H

type LRTree a = List (LPath a)

expand :: forall a b. Semiring b => b -> LPath b -> Context a b -> List (H.Heap b (LPath b))
expand d (LP {unLPath:p}) (_ /\ _ /\ _ /\ s) = map (\(l /\ v)->H.unit (l+d) (LP { unLPath: (v /\ (l+d)):p })) s

--   The edge labels of type @b@ are the edge weights; negative edge
--   weights are not supported.
dijkstra :: forall a b gr. Ord b
    => Semiring b
    => Graph gr
    => H.Heap b (LPath b) -- ^ Initial heap of known paths and their lengths.
    -> gr a b
    -> LRTree b
dijkstra h g | H.isEmpty h || isEmpty g = Nil
dijkstra h g = case H.splitMin h of
      Just (_ /\ p@(LP {unLPath: ((v /\ d):_)}) /\ h') -> case match v g of
         (Just c /\ g') -> p:dijkstra (H.mergeAll (h':expand d p c)) g'
         (Nothing /\ g') -> dijkstra h' g'
      _ -> Nil

--   The edge labels of type @b@ are the edge weights; negative edge
--   weights are not supported.
dijkstraPath :: forall a b gr. Ord b
    => Semiring b
    => Graph gr
    => H.Heap b (LPath b) -- ^ Initial heap of known paths and their lengths.
    -> Node
    -> gr a b
    -> LPath b
dijkstraPath h _ g | H.isEmpty h || isEmpty g = emptyLP
dijkstraPath h dest g = case H.splitMin h of
      Just (_ /\ p@(LP {unLPath: ((v /\ d):_)}) /\ h') -> if v == dest then p else case match v g of
         (Just c /\ g') -> dijkstraPath (H.mergeAll (h':expand d p c)) dest g'
         (Nothing /\ g') -> dijkstraPath h' dest g'
      _ -> emptyLP

-- | Tree of shortest paths from a certain node to the rest of the
--   (reachable) nodes.
--
--   Corresponds to 'dijkstra' applied to a heap in which the only known node is
--   the starting node, with a path of length 0 leading to it.
--
--   The edge labels of type @b@ are the edge weights; negative edge
--   weights are not supported.
spTree :: forall gr. Graph gr
    => Node
    -> gr Int Int
    -> LRTree Int
spTree v g = dijkstra (myHeap v) g

sp :: forall gr. Graph gr
    => Node
    -> Node
    -> gr Int Int
    -> LPath Int
sp src dest g = dijkstraPath (myHeap src) dest g

myHeap :: Int -> H.Heap Int (LPath Int)
myHeap v = H.unit 0 (LP { unLPath: singleton (v /\ 1) })
