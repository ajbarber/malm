module Tree where

import Prelude

import Data.List (concatMap, filter, foldl, (:), List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested (T3, over1, (/\))
import Graph (class DynGraph, class Graph, Adj, Context, Node, insEdges, labNodes, labEdges)
import Tuple (over2')

newtype Gr a b = Gr (GraphRep a b)

type GraphRep a b = Map Node (Context' a b)
type Context' a b = T3 (Adj b) a (Adj b)

type UGr = Gr Unit Unit

----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------

instance showGraph :: (Show a, Show b) => Show (Gr a b) where
  show g =
    let nodes = labNodes g
        edges = labEdges g
    in "Gr[nodes=" <> show nodes <> ", edges=" <> show edges <> "]"

instance graph :: Graph Gr where
  empty             = Gr M.empty

  isEmpty (Gr g)    = M.isEmpty g

  match v gr@(Gr g) = case (M.pop v g) of
    Just (c' /\ m) -> over1 Just (cleanSplit v c' m)
    Nothing -> (Nothing /\ gr)

  mkGraph vs es     = insEdges es
                      $ Gr
                      <<< M.fromFoldable
                      $ map (over2' (\l -> (Nil /\ l /\ Nil))) vs

  labNodes (Gr g)   = map (\(v /\ (_ /\ l /\ _))->(v /\ l)) (M.toUnfoldable g)

  noNodes   (Gr g)  = M.size g

  labEdges  (Gr g)  = concatMap (\(v /\ (_ /\ _ /\ s))->map (\(l /\ w)->(v /\ w /\ l)) s) (M.toUnfoldable g)

-- DynGraph
--
instance dynGraph :: DynGraph Gr where
  merge (p /\ v /\ l /\ s) (Gr g) = Gr
                                    <<< updAdj p (addSucc v)
                                    <<< updAdj s (addPred v)
                                    $ M.alter (const $ Just cntxt') v g
    where
      cntxt' = (p /\ l /\ s)

-- After a Node (with its corresponding Context') are split out of a
-- GraphRep, clean up the remainders.
cleanSplit :: forall a b. Node -> Context' a b -> GraphRep a b
              -> Tuple (Context a b) (Gr a b)
cleanSplit v (p /\l /\ s) g = (c /\ Gr g')
  where
    -- Note: loops are kept only in successor list
    c = (p' /\ v /\ l /\ s)
    p' = rmLoops p
    s' = rmLoops s
    rmLoops = filter ((_/=v) <<< snd)

    g' = updAdj s' (clearPred v) <<< updAdj p' (clearSucc v) $ g

addSucc :: forall a b. Node -> b -> Context' a b -> Context' a b
addSucc v l (p /\  l' /\  s) = (p /\ l' /\ ((l /\ v): s))

addPred :: forall a b. Node -> b -> Context' a b -> Context' a b
addPred v l (p /\ l' /\ s) = (((l /\ v):p) /\ l' /\ s)

clearSucc :: forall a b. Node -> b -> Context' a b -> Context' a b
clearSucc v _ (p /\ l /\ s) = (p /\ l /\ filter ((_/=v) <<< snd) s)

clearPred :: forall a b. Node -> b -> Context' a b -> Context' a b
clearPred v _ (p /\ l /\ s) = (filter ((_/=v) <<< snd) p /\ l /\ s)

updAdj :: forall a b. Adj b -> (b -> Context' a b -> Context' a b) -> GraphRep a b -> GraphRep a b
updAdj adj f g = foldl (\g' (l /\ v) -> M.update (Just <$> f l) v g') g adj
