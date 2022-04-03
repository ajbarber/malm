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

attackFrames :: Number
attackFrames = 180.0

attackCooldown :: Number
attackCooldown = 20.0

data IsAttacking = Start | Cooldown | False

data Direction = Left | Right | Down | Up | None

data DirectionTick = DirectionTick Direction Number

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show = genericShow

instance monoidDirection :: Monoid Direction where
  mempty = None

instance semigroupDirection :: Semigroup Direction where
  append None r = r
  append l _ = l

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

instance monoidAction :: Monoid Action where
  mempty = Default

instance semigroupAction :: Semigroup Action where
  append (Attacking _) (Attacking r) = Attacking r
  append lhs _ = lhs

derive instance eqDirection :: Eq Direction

derive instance eqAction :: Eq Action

derive instance eqIsAttacking :: Eq IsAttacking

derive instance genericDirectionTick :: Generic DirectionTick _

instance showDirectionTick :: Show DirectionTick where
  show = genericShow

derive instance eqDirectionTick :: Eq DirectionTick

instance directionTickMonoid :: Monoid DirectionTick where
  mempty = DirectionTick None 0.0

instance directionTickSemigroup :: Semigroup DirectionTick where
  append (DirectionTick d1 i1) (DirectionTick d2 _) = DirectionTick (d1 <> d2) i1

dec :: DirectionTick -> DirectionTick
dec (DirectionTick d f) = DirectionTick d (f - 1.0)

direction :: DirectionTick -> Direction
direction (DirectionTick a _) = a

speed :: DirectionTick -> Number
speed (DirectionTick _ b) = b

data AnimationType = Damage | Dying

data Action = Default | Attacking Number

type Animation = {
  frames :: Int,
  type_ :: AnimationType
}

type SpriteState = {
  img :: CanvasImageSource,
  location :: Location Coords,
  direction :: InputEvent DirectionTick,
  action :: InputEvent Action,
  health :: Int,
  width :: Number,
  height :: Number,
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

derive instance genericEventType :: Generic EventType _

instance showEventTyp :: Show EventType where
  show = genericShow

derive instance eqEventType :: Eq EventType

type AsyncState =  Array (Tuple String EventType)

data InputEvent e = InputEvent e

instance inputEventShow :: (Show e) => Show (InputEvent e)  where
  show (InputEvent e) = "Input Event "<> show e

derive instance inputEvent :: (Eq e) => Eq (InputEvent e)

instance inputEventMonoid :: (Eq e, Monoid e) => Monoid (InputEvent e) where
   mempty = InputEvent mempty

instance inputEventSemigroup :: (Eq e, Monoid e) => Semigroup (InputEvent e) where
  append (InputEvent e1) (InputEvent e2) = InputEvent (e1 <> e2)

instance functorInputEvent :: Functor (InputEvent) where
  map f (InputEvent e) = InputEvent (f e)

key ::
  forall e.
  InputEvent e ->
  e
key (InputEvent e) = e

data Vertex = Vertex Number Number

type Coords = ICoords Offset

type CoordsIn = ICoords ()

type ICoords a = { w :: Number,
                   h :: Number,
                   xpos :: Number,
                   ypos :: Number | a }

type Offset = (xoffset :: Number, yoffset :: Number, perimeter :: Number)

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

attackState :: SpriteState -> IsAttacking
attackState s = case key s.action of
  Attacking x -> if x >= attackFrames - attackCooldown then Start else Cooldown
  _ -> False

isAttacking :: SpriteState -> Boolean
isAttacking s = attackState s == Start ||  attackState s == Cooldown
