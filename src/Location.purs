module Location where

import Prelude

import Data.Array (any)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Event (direction)
import Math ((%), floor)
import Types (Coords, Cut, Direction(..), Location(..), Source, SpriteState, State, dest, slot)

dampen :: Number -> Location Coords -> Source
dampen frame location = do
  src { xpos = src.xpos + (dampF frame) % 2.0 * src.w }
  where
    dampF x = x - x % 9.0
    Location src dst = location

toCut ::
  Location Coords ->
  Array Direction ->
  Cut Coords ->
  Location Coords
toCut loc dir cuts = let src = slot cuts (direction dir) in
  Location src (dest loc)

offset :: Coords -> Coords -> Tuple Number Number
offset hero world = Tuple trans1 trans2
  where
    trans1 = world.xpos  - (floor hero.xoffset)
    trans2 = world.ypos  - (floor hero.yoffset)

offsetX :: Coords -> Number
offsetX coords = coords.xpos + coords.xoffset

offsetY :: Coords -> Number
offsetY coords = coords.ypos + coords.yoffset

position :: Milliseconds -> SpriteState -> Coords
position (Milliseconds delta') st = let
  coords = dest st.location in
  case (direction st.direction) of
    Left -> coords { xoffset = coords.xoffset - delta' }
    Right -> coords { xoffset = coords.xoffset + delta' }
    Up -> coords { yoffset = coords.yoffset - delta' }
    Down -> coords { yoffset = coords.yoffset + delta' }
    None -> coords

isCollision :: State -> Coords -> Boolean
isCollision st loc = any (\x -> collision loc (dest x.loc)) st.tileMap.walls

isBoundary :: State -> Coords -> Boolean
isBoundary st loc =
  offsetX loc <=  st.tileMap.xMin ||
  offsetY loc <=  st.tileMap.yMin ||
  offsetX loc >=  st.tileMap.xMax ||
  offsetY loc >= st.tileMap.yMax

collision :: Coords -> Coords -> Boolean
collision loc tileLoc = xCollision && yCollision
   where
     xCollision = offsetX loc + loc.w > tileLoc.xpos &&
                  (offsetX loc < tileLoc.xpos + tileLoc.w)
     yCollision = offsetY loc + loc.h > tileLoc.ypos &&
                  (offsetY loc < tileLoc.ypos + tileLoc.h)
