module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Test.Graph (testGraphs)
import Test.GraphRep (testFromNode1, testFromNode2, testFromNode3, testFromNode4a, testFromNode4b, testFromNode4c, testSp, testToGraph, testToNode1, testToNode2, testToNode3)

main :: Effect Unit
main = do
 testGraphs
 testToNode1
 testFromNode1
 testToNode2
 testFromNode2
 testToNode3
 testFromNode3
 testFromNode4a
 testFromNode4b
 testFromNode4c
 testToGraph
 testSp
