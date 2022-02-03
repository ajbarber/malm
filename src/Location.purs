module Location where

import Prelude

import Data.Array (any)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Event (direction)
import Math ((%), floor)
import Types (Coords, Cut, Direction(..), Location(..), Source, SpriteState, State, Vertex(..), dest, slot, toVertices)

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

isBlocked :: State -> Coords -> Boolean
isBlocked st loc = isObstacle st loc || isBoundary st loc

isObstacle :: State -> Coords -> Boolean
isObstacle st loc = any (\x -> collision loc (dest x.location) loc.xoffset loc.yoffset) st.tileMap.walls

isCollision :: State -> Coords -> Boolean
isCollision st loc = let npcLoc = dest st.npc.location in
  collision2 (toVertices loc) npcLoc 0.0 0.0

isBoundary :: State -> Coords -> Boolean
isBoundary st loc =
  offsetX loc <=  st.tileMap.xMin ||
  offsetY loc <=  st.tileMap.yMin ||
  offsetX loc >=  st.tileMap.xMax ||
  offsetY loc >= st.tileMap.yMax

-- Note that offset is applied to the first location passed
collision :: Coords -> Coords -> Number -> Number -> Boolean
collision loc1 loc2 xoff yoff = xCollision && yCollision
   where
     xCollision = loc1.xpos + xoff + loc1.w > loc2.xpos &&
                  (loc1.xpos + xoff < loc2.xpos + loc2.w)
     yCollision = loc1.ypos + yoff + loc1.h > loc2.ypos &&
                  (loc1.ypos + yoff < loc2.ypos + loc2.h)

-- tests if any of the vertices supplied fall within the rectangle defined by
-- loc xoff yoff
collision2 :: Array Vertex -> Coords -> Number -> Number -> Boolean
collision2 vertices loc xoff yoff =
  let loc' = loc { xpos = loc.xpos - xoff, ypos = loc.ypos - yoff } in
  any (hasVertex loc') vertices

hasVertex :: Coords -> Vertex -> Boolean
hasVertex { xpos, ypos, w, h} (Vertex x y) =
  x < xpos + w && x > xpos && y < ypos + h && y > ypos
