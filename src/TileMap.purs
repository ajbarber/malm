module TileMap where

import Prelude

import Data.Int (toNumber)
import Data.Number.Format (toString)
import Types (Location(..), TileData, TileMap(..))

formatInt :: Int -> String
formatInt i = case (i < 10) of
  true -> "0" <> asStr i
  false -> asStr i
  where
    asStr = toString <<< toNumber

tileMap :: TileMap
tileMap = TileMap "assets" tileData

tileData :: Array TileData
tileData = [ { file: "Overworld.png",
               loc: Location  { w: 16.0, h: 16.0, xpos: 0.0, ypos: 0.0 }
                              { w: 16.0, h: 16.0, xpos: 0.0, ypos: 0.0 },
               wall: false},
             { file: "Overworld.png",
               loc: Location { w: 80.0, h: 96.0, xpos: 96.0, ypos: 0.0}
                             { w: 80.0, h: 96.0, xpos: 0.0, ypos: 48.0},
               wall: true }
           ]
