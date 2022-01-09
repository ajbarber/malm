module Types where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Array.NonEmpty (singleton, NonEmptyArray)
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Tuple (Tuple)
import P5.Types (Image, P5, ElementOrImage)

data Direction = Left | Right | Down | Up | None

derive instance eqDirection :: Eq Direction

type EventTick = Array Direction

type PreloadState = { p :: P5,
                      hero :: Image,
                      tileMap :: Array LoadedTile }

type AsyncState = { event :: EventTick,
                    location :: Location Coords }

data GameState = GameState PreloadState AsyncState

type Coords = ICoords Offset

type CoordsIn = ICoords ()

type ICoords a = { w :: Number,
                   h :: Number,
                   xpos :: Number,
                   ypos :: Number | a }

type Offset = (xoffset :: Number, yoffset :: Number)

type InputCoordMap = { src :: CoordsIn, dest :: CoordsIn, wall :: Boolean }

coordFromJson :: Json -> Either JsonDecodeError InputCoordMap
coordFromJson = decodeJson

type Dest = Coords
type Source = Coords

data Location a = Location a a

derive instance locationFunctor :: Functor Location

type TileData = { file :: String,
                  loc :: Location Coords,
                  wall :: Boolean }

data Cut a = Cut a a a a

derive instance cutFunctor :: Functor Cut

type LoadedTile = { e :: ElementOrImage, loc :: Location Coords, wall :: Boolean }

data TileMap = TileMap String (Array TileData)

dest :: Location Coords -> Coords
dest (Location _ d) = d

source :: Location Coords -> Coords
source (Location s _) = s

slot :: Cut Coords -> Direction -> Source
slot (Cut l r u d) dir = case dir of
  Left -> l
  Right -> r
  Up -> u
  Down -> d
  None-> d
