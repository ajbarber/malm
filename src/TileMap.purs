module TileMap where

import Prelude

import Data.Array (range)
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
tileData = grassRow 16 0.0 <>
           grassRow 16 16.0 <>
           [ grass2 164.0 0.0,
             rocks 156.0 0.0,
             grass2 228.0 0.0,
             grass2 268.0 0.0,
             rocks2 0.0 15.0,
             stream 0.0 32.0,
             stream 32.0 32.0,
             { file: "Overworld.png",
               loc: Location { w: 80.0, h: 96.0, xpos: 96.0, ypos: 0.0}
                             { w: 80.0, h: 96.0, xpos: 0.0, ypos: 48.0},
               wall: true }
           ]

grassRow :: Int -> Number -> Array TileData
grassRow width y = map (\i -> grass i y) xs
  where
     xs = map (\x -> (toNumber x) * 16.0) $ range 0 width

grass :: Number -> Number -> TileData
grass x y = { file: "Overworld.png",
          loc: Location  { w: 16.0, h: 16.0, xpos: 0.0, ypos: 0.0 }
                         { w: 16.0, h: 16.0, xpos: x, ypos: y },
          wall: false}

grass2 :: Number -> Number -> TileData
grass2 x y = { file: "Overworld.png",
          loc: Location  { w: 48.0, h: 48.0, xpos: 224.0, ypos: 464.0 }
                         { w: 48.0, h: 48.0, xpos: x, ypos: y },
          wall: false}

rocks2 :: Number -> Number -> TileData
rocks2 x y = { file: "Overworld.png",
               loc: Location
               { w: 48.0, h: 16.0, xpos: 96.0, ypos: 80.0 }
               { w: 48.0, h: 16.0, xpos: x, ypos: y },
               wall: true}


rocks :: Number -> Number -> TileData
rocks x y = { file: "Overworld.png",
              loc: Location
              { w: 30.0, h: 30.0, xpos: 64.0, ypos: 16.0 }
              { w: 30.0, h: 30.0, xpos: x, ypos: y },
                wall: true}

stream :: Number -> Number -> TileData
stream x y = { file: "Overworld.png",
               loc: Location
              { w: 32.0, h: 32.0, xpos: 0.0, ypos: 16.0 }
              { w: 32.0, h: 32.0, xpos: x, ypos: y },
                wall: true}
