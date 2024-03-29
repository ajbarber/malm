module Types where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Graph (class Graph)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Math ((%))
import Tree (Gr(..))

attackFrames :: Number
attackFrames = 180.0

attackCooldown :: Number
attackCooldown = 60.0

data IsAttacking = Start | Cooldown | False

data Direction = Left | Right | Down | Up | None

data DirectionTick = DirectionTick Direction Number

type Distance = Number

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

instance functorMovement :: Functor Movement where
  map f (InputMovement d) = InputMovement (map f d)
  map f (PathMovement p) = PathMovement (map f p)

foldMovement :: forall a b. (a -> b) -> Movement a -> Maybe b
foldMovement f movement = case movement of
  InputMovement (InputEvent d) -> Just (f d)
  PathMovement (Path d _ _) -> Just (f d)
  PathMovement End -> Nothing

-- Movement is either some input event describing some motion type a
-- or a path we've calculated for the sprite
data Movement a = InputMovement (InputEvent a) | PathMovement (Path a)

data Path a = Path a Distance (Path a) | End

data SpriteType = Hero | Npc

derive instance eqMovement :: Eq a => Eq (Movement a)

derive instance eqPath :: Eq a => Eq (Path a)

instance pathShow :: (Show a) => Show (Path a)  where
  show (Path a d p) = "[" <> show a <> ", Distance:" <> show d <> "]" <> show p
  show End = "End"

instance functorPath :: Functor Path where
  map f (Path a d p) = Path (f a) d (map f p)
  map _ End = End

type SpriteState = {
  typ :: SpriteType,
  img :: CanvasImageSource,
  location :: Location Coords,
  direction :: Movement DirectionTick,
  action :: InputEvent Action,
  health :: Int,
  width :: Number,
  height :: Number,
  animation :: Maybe Animation
}

type State = { ctx :: Context2D,
               tileMap :: LoadedTileMap,
               graph :: Gr Int Int,
               deltaTime :: Milliseconds,
               frameCount :: Int,
               scene :: Scene,
               hero :: SpriteState,
               npc :: Array SpriteState
             }

data Scene = Main | Dead Int

derive instance eqScene :: Eq Scene

data EventType = KeyDown | KeyUp

derive instance genericEventType :: Generic EventType _

instance showEventTyp :: Show EventType where
  show = genericShow

derive instance eqEventType :: Eq EventType

type AsyncState =  Array (Tuple String EventType)

data InputEvent e = InputEvent e

instance inputEventShow :: (Show e) => Show (InputEvent e)  where
  show (InputEvent e) = "Input Event "<> show e

-- instance coordsShow :: Show (ICoords a) where
--   show { w, h, xpos, ypos } = "w: " <> w <> "," <> "h: " <> h <> "xpos: " <> xpos <> "," <> "ypos" <> ypos

derive instance inputEvent :: (Eq e) => Eq (InputEvent e)

instance inputEventMonoid :: (Eq e, Monoid e) => Monoid (InputEvent e) where
   mempty = InputEvent mempty

instance inputEventSemigroup :: (Eq e, Monoid e) => Semigroup (InputEvent e) where
  append (InputEvent e1) (InputEvent e2) = InputEvent (e1 <> e2)

instance functorInputEvent :: Functor (InputEvent) where
  map f (InputEvent e) = InputEvent (f e)

instance pathMonoid :: (Eq e, Monoid e) => Monoid (Path e) where
   mempty = End

instance pathSemigroup :: (Eq e, Monoid e) => Semigroup (Path e) where
  append (Path a1 d1 r1) _ = Path a1 d1 r1
  append End _ = End

instance movementMonoid :: (Eq e, Monoid e) => Monoid (Movement e) where
   mempty = InputMovement (InputEvent mempty)

instance movementSemigroup :: (Eq e, Monoid e) => Semigroup (Movement e) where
  append (InputMovement e1) (InputMovement e2) = InputMovement (e1 <> e2)
  append (InputMovement e1) (PathMovement End) = InputMovement e1
  append (InputMovement _) (PathMovement e2) = PathMovement e2
  append (PathMovement e1) _ = PathMovement e1

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

data Neighbours = CoordsIn CoordsIn CoordsIn CoordsIn

derive instance cutFunctor :: Functor Cut

type LoadedTileA a = { location :: Location Coords, wall :: Boolean | a}
type LoadedTile = LoadedTileA (img :: CanvasImageSource)

data TileMap = TileMap String (Array TileData)

type LoadedTileMap = {
  xMin :: Number,
  yMin :: Number,
  xMax :: Number,
  yMax :: Number,
  tiles :: Array LoadedTile,
  walls :: Array Coords}

dest :: Location Coords -> Coords
dest (Location _ d) = d

source :: Location Coords -> Coords
source (Location s _) = s

toCoords :: forall a. LoadedTileA a -> Coords
toCoords tile = dest tile.location

slot :: Cut Coords -> Maybe Direction -> Source
slot (Cut l r u d) (Just dir) = case dir of
  Left -> l
  Right -> r
  Up -> u
  Down -> d
  None-> d
slot (Cut l r u d) Nothing = d

reverseTick :: DirectionTick -> DirectionTick
reverseTick (DirectionTick dir r) = DirectionTick (reverse dir) r

reverse :: Direction -> Direction
reverse Up = Down
reverse Down = Up
reverse Left = Right
reverse Right = Left
reverse None = None

clockwise :: Direction -> Direction
clockwise Up = Right
clockwise Down = Left
clockwise Left = Up
clockwise Right = Down
clockwise None = None

randomDir :: State -> Direction -> Direction
randomDir state _ = let x = (toNumber state.frameCount) % 4.0 in case x of
  1.0 -> Left
  2.0 -> Right
  3.0 -> Up
  4.0 -> Down
  _ -> None

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
