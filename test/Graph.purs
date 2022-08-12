module Test.Graph where

import Prelude

import Data.List (fromFoldable)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Graph (LNode, LEdge, mkGraph)
import SP (spTree, sp)
import Tree (Gr)

mkGraph' :: forall a b. Array (LNode a) -> Array (LEdge b) -> Gr a b
mkGraph' vs es = mkGraph (fromFoldable vs) (fromFoldable es)

exampleFlowGraph1 :: Gr Unit Int
exampleFlowGraph1 = mkGraph' [ (1 /\ unit), (2 /\ unit), (3 /\ unit), (4 /\ unit), (5 /\ unit), (6 /\ unit) ]
                             [ (1 /\ 2 /\ 1000), (1 /\ 3 /\ 1000), (3 /\ 6 /\ 100)
                             , (2 /\ 3 /\ 1), (2 /\ 5 /\ 1000), (3 /\ 4 /\ 1000), (4 /\ 5 /\ 100)
                             ]

exampleDistanceGraph1 :: Gr Int Int
exampleDistanceGraph1 = mkGraph' [ (1 /\ 10), (2 /\ 10), (3 /\ 10), (4 /\ 10), (5 /\ 10), (6 /\ 10) ]
                [ (1 /\ 2 /\ 1000), (1 /\ 3 /\ 1000), (3 /\ 6 /\ 100)
                , (2 /\ 3 /\ 1), (2 /\ 5 /\ 1000), (3 /\ 4 /\ 1000), (4 /\ 5 /\ 100) ]

testGraphs :: Effect Unit
testGraphs = do
  log "Example flow graph"
  logShow $ spTree 1 exampleDistanceGraph1
  logShow $ sp 1 6 exampleDistanceGraph1
  pure unit
