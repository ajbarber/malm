module Test.GraphRep where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import GraphRep (fromNode, toGraph, toNode)
import SP (sp)
import Types (Location(..))

testToNode1 :: Effect Unit
testToNode1 = do
  logShow $ "toNode xMax 64, (xpos 16, ypos 32): " <> show (toNode 64.0 (16.0 /\ 32.0))

testFromNode1 ∷ ∀ (t11 ∷ Type -> Type). MonadEffect t11 ⇒ t11 Unit
testFromNode1 = do
  logShow $ "fromNode xMax 24 node: 9" <> show (fromNode 64.0 9)

testToNode2 :: Effect Unit
testToNode2 = do
  logShow $ "toNode xMax 192, (xpos 176, ypos 16): " <> show (toNode 192.0 (176.0 /\ 16.0))

testFromNode2 ∷ ∀ (t11 ∷ Type -> Type). MonadEffect t11 ⇒ t11 Unit
testFromNode2 = do
  logShow $ "fromNode xMax 192 node: 23" <> show (fromNode 192.0 23)

-- each row 976/16 = 61 squares
--101 = 6 full rows + 71/16 = 4.43 =~ 4 squares from left.
testToNode3 :: Effect Unit
testToNode3 = do
  logShow $ "toNode xMax 976, (xpos 71, ypos 101): " <> show (toNode 976.0 (71.0 /\ 101.0))

testFromNode3 ∷ ∀ (t11 ∷ Type -> Type). MonadEffect t11 ⇒ t11 Unit
testFromNode3 = do
  logShow $ "fromNode xMax 976 node: 370" <> show (fromNode 976.0 370)

-- moving vertically upwards
testFromNode4a ∷ ∀ (t11 ∷ Type -> Type). MonadEffect t11 ⇒ t11 Unit
testFromNode4a = do
  logShow $ "fromNode xMax 976 node: 488" <> show (fromNode 976.0 488)

-- moving vertically upwards agani
testFromNode4b ∷ ∀ (t11 ∷ Type -> Type). MonadEffect t11 ⇒ t11 Unit
testFromNode4b = do
  logShow $ "fromNode xMax 976 node: 427" <> show (fromNode 976.0 427)

testFromNode4c ∷ ∀ (t11 ∷ Type -> Type). MonadEffect t11 ⇒ t11 Unit
testFromNode4c = do
  logShow $ "fromNode xMax 976 node: 497" <> show (fromNode 976.0 497)

dests ∷ Array { h ∷ Number , perimeter ∷ Number , w ∷ Number , xoffset ∷ Number , xpos ∷ Number , yoffset ∷ Number , ypos ∷ Number }
dests =[ { xpos: 0.0, ypos: 0.0, w: 16.0, h: 16.0, perimeter: 0.0, xoffset: 0.0, yoffset: 0.0 },
         { xpos: 0.0, ypos: 16.0, w: 16.0, h: 16.0, perimeter: 0.0, xoffset: 0.0, yoffset: 0.0},
         { xpos: 176.0, ypos: 16.0, w: 16.0, h: 16.0, perimeter: 0.0, xoffset: 0.0, yoffset: 0.0}
       ]

testTiles ∷ Array { location ∷ Location { h :: Number , perimeter :: Number , w :: Number , xoffset :: Number , xpos :: Number , yoffset :: Number , ypos :: Number } , wall ∷ Boolean }
testTiles = (\l -> { location: l, wall:false}) <$> map (\d -> Location d d) dests

testToGraph :: Effect Unit
testToGraph = logShow $ show $ toGraph 192.0 testTiles

testSp :: Effect Unit
testSp = logShow $ show $ sp 0 24 $ toGraph 192.0 testTiles
