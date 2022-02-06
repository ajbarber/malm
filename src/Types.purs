module Types where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Web.Event.Event (Event)

data Direction = Left | Right | Down | Up | None

derive instance eqDirection :: Eq Direction

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show = genericShow

type DirectionTick = Array Direction

data AnimationType = Damage | Dying

type Animation = {
  frames :: Int,
  type_ :: AnimationType
}

type SpriteState = {
  img :: CanvasImageSource,
  location :: Location Coords,
  direction :: DirectionTick,
  health :: Int,
  animation :: Maybe Animation
}

type State = { ctx :: Context2D,
               tileMap :: LoadedTileMap,
               deltaTime :: Milliseconds,
               frameCount :: Int,
               scene :: Scene,
               hero :: SpriteState,
               npc :: SpriteState
             }

data Scene = Main | Dead Int

data EventType = KeyDown | KeyUp

derive instance eqEventType :: Eq EventType

type AsyncState =  Array (Tuple Event EventType)

data Vertex = Vertex Number Number

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

type LoadedTile = { img :: CanvasImageSource, location :: Location Coords, wall :: Boolean }

data TileMap = TileMap String (Array TileData)

type LoadedTileMap = {
  xMin :: Number,
  yMin :: Number,
  xMax :: Number,
  yMax :: Number,
  tiles :: Array LoadedTile,
  walls :: Array LoadedTile}

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

reverse :: Direction -> Direction
reverse Up = Down
reverse Down = Up
reverse Left = Right
reverse Right = Left
reverse None = None

toVertices :: Coords -> Array Vertex
toVertices { xpos, ypos, h, w } = [
  Vertex xpos ypos,
  Vertex xpos (ypos + h),
  Vertex (xpos + w) ypos,
  Vertex (xpos + w) (ypos + h) ]
