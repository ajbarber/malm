module Test.Main where

import Prelude

import Effect (Effect)
import Test.Graph (testGraphs)
import Test.GraphRep (testFromNode1, testFromNode2, testFromNode3, testFromNode4a, testFromNode4b, testFromNode4c, testSp, testTilesGen, testToGraph, testToNode1, testToNode2, testToNode3)
import Test.Path (testToPath)

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
 testToPath
 testTilesGen
