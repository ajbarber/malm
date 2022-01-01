module TileMap where

import Prelude

import Data.Int (toNumber)
import Data.Number.Format (toString)
import P5.Types (ElementOrImage)

formatInt :: Int -> String
formatInt i = case (i < 10) of
  true -> "0" <> asStr i
  false -> asStr i
  where
    asStr = toString <<< toNumber

type TileData = { file :: String,
              loc :: TileLoc,
              wall :: Boolean }

type TileLoc ={ w :: Number,
                h :: Number,
                xpos :: Number,
                ypos :: Number }

type LoadedTile = { e :: ElementOrImage, loc :: TileLoc }

data TileMap = TileMap String (Array TileData)

tileMap :: TileMap
tileMap = TileMap "assets/PNG/Retina/Environment" tileData

tileData :: Array TileData
tileData = [ { file: "scifiEnvironment_01.png",
              loc: { w: 64.0, h: 64.0, xpos: 64.0, ypos: 64.0 },
              wall: false},
           { file: "scifiEnvironment_06.png",
              loc: { w: 64.0, h: 64.0, xpos: 100.0, ypos:64.0 },
              wall: false }
          ]
