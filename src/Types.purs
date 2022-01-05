module Types where

import Data.Array.NonEmpty (singleton, NonEmptyArray)
import Data.Tuple (Tuple)
import Data.Eq (class Eq)

import P5.Types (Image, P5, ElementOrImage)

data Direction = Left | Right | Down | Up | None

derive instance eqDirection :: Eq Direction

type EventTick = Array Direction

type PreloadState = { p :: P5,
                      hero :: Image,
                      tileMap :: Array LoadedTile }

type AsyncState = { event :: EventTick,
                    location :: Location }

data GameState = GameState PreloadState AsyncState

type Coords ={ w :: Number,
               h :: Number,
               xpos :: Number,
               ypos :: Number }

type Dest = Coords
type Source = Coords

data Location = Location Source Dest

type TileData = { file :: String,
                  loc :: Location,
                  wall :: Boolean }

data Cut = Cut Coords Coords Coords Coords

type LoadedTile = { e :: ElementOrImage, loc :: Location, wall :: Boolean }

data TileMap = TileMap String (Array TileData)

dest :: Location -> Coords
dest (Location _ d) = d

source :: Location -> Coords
source (Location s _) = s

slot :: Cut -> Direction -> Source
slot (Cut l r u d) dir = case dir of
  Left -> l
  Right -> r
  Up -> u
  Down -> d
  None-> d
