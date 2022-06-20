module Location where

import Prelude

import Data.Array (any)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Math ((%), floor)
import Types (Coords, Cut, Direction(..), DirectionTick(..), InputEvent(..), Location(..), Movement(..), Source, SpriteState, State, Vertex(..), dest, key, slot, toVertices)

dampen ::
  Maybe Number ->
  Maybe Number ->
  Number ->
  Location Coords ->
  Number ->
  Source
dampen width factor frame location ncuts = do
  src { xpos = src.xpos + (dampF frame) % ncuts * (fromMaybe src.w width) }
  where
    dampF x = x - x % (fromMaybe 9.0 factor)
    Location src dst = location

toCut ::
  Location Coords ->
  Maybe Direction ->
  Cut Coords ->
  Location Coords
toCut loc dir cuts = let src = slot cuts dir in
  Location src (dest loc)

offset :: Coords -> Coords -> Coords
offset hero other = other { xpos=trans1, ypos=trans2 }
  where
    trans1 = other.xpos  - (floor hero.xoffset)
    trans2 = other.ypos  - (floor hero.yoffset)

position :: SpriteState -> Coords
position st = let
  coords = dest st.location in
  case st.direction of
    InputMovement ied -> inputDrivenPosition ied coords
    PathMovement p -> { h: 0.0, perimeter: 0.0, w: 0.0,
                        xoffset:0.0, yoffset: 0.0,  xpos: 0.0, ypos: 0.0}

inputDrivenPosition :: InputEvent DirectionTick -> Coords -> Coords
inputDrivenPosition ied src = case (key ied) of
    DirectionTick Left f -> src { xoffset = src.xoffset - f }
    DirectionTick Right f -> src { xoffset = src.xoffset + f }
    DirectionTick Up f -> src { yoffset = src.yoffset - f }
    DirectionTick Down f -> src { yoffset = src.yoffset + f }
    DirectionTick None _ -> src

movement :: SpriteState -> Tuple Coords Coords
movement ss = Tuple (dest ss.location) (position ss)

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

isObstacle :: State -> Coords -> Boolean
isObstacle st loc = isObstacle' st loc { xpos = loc.xpos + loc.xoffset,
                                         ypos = loc.ypos + loc.yoffset }

isObstacle' :: State -> Coords -> Boolean
isObstacle' st loc = any (\x -> collision loc (dest x.location)) st.tileMap.walls

-- isCollision :: State -> Coords -> Boolean
-- isCollision st loc = isCollision' st $ loc { xpos = loc.xpos + loc.xoffset,
--                                              ypos = loc.ypos + loc.yoffset }

-- isCollision' :: State -> Coords -> Boolean
-- isCollision' st loc = let npcLoc = dest (st.npc.location) in
--   collision2 (toVertices loc) npcLoc

collision' :: Coords -> Coords -> Boolean
collision' loc1 loc2 = collision2 (toVertices (off loc1)) (off loc2)
  where
    off l  = l{xpos = l.xpos + l.xoffset + l.perimeter,
               ypos = l.ypos + l.yoffset + l.perimeter }

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
hasVertex { perimeter, xpos, ypos, w, h} (Vertex x y) =
  x < xpos + w + perimeter &&
  x > xpos - perimeter &&
  y < ypos + h + perimeter &&
  y > ypos - perimeter
