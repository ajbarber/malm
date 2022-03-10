module Location where

import Prelude

import Data.Array (any)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Event (direction)
import Math ((%), floor)
import Types (Coords, Cut, Direction(..), Location(..), Source, SpriteState, State, Vertex(..), dest, key, slot, source, toVertices)

dampen :: Number -> Location Coords -> Source
dampen frame location = do
  src { xpos = src.xpos + (dampF frame) % 2.0 * src.w }
  where
    dampF x = x - x % 9.0
    Location src dst = location

toCut ::
  Location Coords ->
  Direction ->
  Cut Coords ->
  Location Coords
toCut loc dir cuts = let src = slot cuts dir in
  Location src (dest loc)

offset :: Coords -> Coords -> Coords
offset hero other = other { xpos=trans1, ypos=trans2 }
  where
    trans1 = other.xpos  - (floor hero.xoffset)
    trans2 = other.ypos  - (floor hero.yoffset)

position :: Milliseconds -> SpriteState -> Coords
position (Milliseconds delta') st = let
  coords = dest st.location in
  case (key st.direction) of
    Left -> coords { xoffset = coords.xoffset - delta' }
    Right -> coords { xoffset = coords.xoffset + delta' }
    Up -> coords { yoffset = coords.yoffset - delta' }
    Down -> coords { yoffset = coords.yoffset + delta' }
    None -> coords

-- Translates passed sprite coordinates according to where our hero is.
translate :: State -> Coords -> Coords
translate state coords = translated { xpos = translated.xpos + translated.xoffset,
                                      ypos = translated.ypos + translated.yoffset }
  where
    translated = offset (dest state.hero.location) coords

translate' :: State -> Coords -> Coords
translate' state coords = translated { xpos = translated.xpos + translated.xoffset,
                                       ypos = translated.ypos + translated.yoffset }
  where
    translated = offset coords (dest state.hero.location)

-- isBlocked :: State -> Coords -> Boolean
-- isBlocked st loc = isBlocked' st (translate st loc)

-- isBlocked' :: State -> Coords -> Boolean
-- isBlocked' st loc = isObstacle' st loc || isBoundary' st loc

isObstacle :: State -> Coords -> Boolean
isObstacle st loc = isObstacle' st loc { xpos = loc.xpos + loc.xoffset,
                                         ypos = loc.ypos + loc.yoffset }

-- isObstacle :: State -> Coords -> Boolean
-- isObstacle st loc = isObstacle' (st { tileMap { walls = walls' st}}) loc
--  where
--    walls' s = map (\w -> w{ location = Location (source w.location) (d' s w)}) s.tileMap.walls
--    d' s w = translate s $ dest w.location

isObstacle' :: State -> Coords -> Boolean
isObstacle' st loc = any (\x -> collision loc (dest x.location)) st.tileMap.walls

isCollision :: State -> Coords -> Boolean
isCollision st loc = isCollision' st $ loc { xpos = loc.xpos + loc.xoffset,
                                             ypos = loc.ypos + loc.yoffset }

isCollision' :: State -> Coords -> Boolean
isCollision' st loc = let npcLoc = dest (st.npc.location) in
  collision2 (toVertices loc) npcLoc

collision' :: Coords -> Coords -> Boolean
collision' loc1 loc2 = collision2 (toVertices (off loc1)) (off loc2)
  where
    off l  = l{xpos = l.xpos + l.xoffset,
               ypos = l.ypos + l.yoffset}

-- isBoundary :: State -> Coords -> Boolean
-- isBoundary st loc = isBoundary' st loc

-- isBoundary' :: State -> Coords -> Boolean
-- isBoundary' st loc =
--   loc.xpos <= st.tileMap.xMin ||
--   loc.ypos <= st.tileMap.yMin ||
--   loc.xpos >= st.tileMap.xMax ||
--   loc.ypos >= st.tileMap.yMax

-- Note that offset is applied to the first location passed
collision :: Coords -> Coords -> Boolean
collision loc1 loc2  = xCollision && yCollision
   where
     xCollision = loc1.xpos + loc1.w > loc2.xpos &&
                  (loc1.xpos < loc2.xpos + loc2.w)
     yCollision = loc1.ypos + loc1.h > loc2.ypos &&
                  (loc1.ypos < loc2.ypos + loc2.h)

-- tests if any of the vertices supplied fall within the rectangle defined by
-- loc xoff yoff
collision2 :: Array Vertex -> Coords -> Boolean
collision2 vertices loc = any (hasVertex loc) vertices

hasVertex :: Coords -> Vertex -> Boolean
hasVertex { xpos, ypos, w, h} (Vertex x y) =
  x < xpos + w && x > xpos && y < ypos + h && y > ypos
