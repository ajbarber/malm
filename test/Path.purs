module Test.Path where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Graph (LPath)
import GraphRep (toGraph')
import Path (toPath)
import SP (sp)
import Test.GraphRep (testTiles)

path ∷ LPath Int
path = sp 0 24 $ toGraph' 192.0 16.0 []

testToPath :: Effect Unit
testToPath = do
  logShow $ "toPath:  " <> show (toPath 192.0 path)
