module Location where

import Prelude

import Data.Array (any)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Math (abs, floor, (%))
import Record as Record
import Type.Prelude (Proxy(..))
import Types (Coords, Cut, Direction(..), DirectionTick(..), InputEvent(..), Location(..), Movement(..), Path(..), Source, SpriteState, SpriteType(..), State, Vertex(..), dest, key, slot, toVertices)

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

distance :: Coords -> Coords -> Number
distance p1 p2 = trans1 + trans2
  where
    trans1 = abs $ (p2.xpos + p2.xoffset)  - (p1.xpos + p1.xoffset)
    trans2 = abs $ (p2.ypos + p2.yoffset) - (p1.ypos + p1.yoffset)

-- distance without offset
distance' :: Coords -> Coords -> Number
distance' p1 p2 = trans1 + trans2
  where
    trans1 = abs $ p2.xpos  - p1.xpos
    trans2 = abs $ p2.ypos - p1.ypos

position :: SpriteState -> Coords
position st = let
  coords = dest st.location in
  case st.direction of
    InputMovement ied -> inputDrivenPosition st.typ (key ied) coords
    PathMovement p -> walkPath st.typ p coords

walkPath :: SpriteType -> Path DirectionTick -> Coords -> Coords
walkPath typ p curCoords = case p of
  Path dirTick _ p -> inputDrivenPosition typ dirTick curCoords
  End -> curCoords

-- direction first passed coords are approaching second passed coords from
side :: Coords -> Coords -> Direction
side coords1 coords2 = case (abs xdiff > abs ydiff) of
  true -> if xdiff < 0.0 then Left else Right
  false -> if ydiff < 0.0 then Up else Down
  where
    xdiff = coords1.xpos + coords1.xoffset - coords2.xpos - coords2.xoffset
    ydiff = coords1.ypos + coords1.yoffset - coords2.ypos - coords2.yoffset

dist :: DirectionTick -> Number
dist (DirectionTick _ f) = f

-- | For the hero only, we keep them at the centre and move the tiles about in the opposite
--   direction. Doing this with Record.modify was difficult as the type checker
-- doesn't seem to know that (Proxy :: Proxy "xpos") and (Proxy :: Proxy "xoffset") are the
-- same type?
inputDrivenPosition :: SpriteType -> DirectionTick -> Coords -> Coords
inputDrivenPosition typ dir src = case dir of
    DirectionTick Left f -> lr (-f)
    DirectionTick Right f -> lr f
    DirectionTick Up f -> ud (-f)
    DirectionTick Down f -> ud f
    DirectionTick None _ -> src
    where
      ud = case typ of
        Hero -> \a -> src{ yoffset = src.yoffset + a}
        _ -> \a -> src{ ypos = src.ypos + a}
      lr = case typ of
        Hero -> \a -> src{ xoffset = src.xoffset + a}
        _ -> \a -> src{ xpos = src.xpos + a}

coordsToDirection :: Coords -> Coords -> Tuple DirectionTick Number
coordsToDirection from to = let
  yDist = to.ypos - from.ypos
  xDist = to.xpos - from.xpos
  dir = if yDist > 0.0 then Up
        else if yDist < 0.0 then Down
        else if xDist > 0.0 then Right
        else if xDist < 0.0 then Left
        else None in
  Tuple (DirectionTick dir 1.0) (abs $ yDist + xDist)

movement :: SpriteState -> Coords
movement ss = position ss

snap :: Tuple Number Number -> Tuple Number Number
snap (x /\ y) = ((x - x % 16.0) /\ (y - y % 16.0))

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
isObstacle' st loc = any (collision loc) st.tileMap.walls

-- | Each Coords defines a square. Checks if any of the vertices of loc1 fall in loc2
collision' :: Coords -> Coords -> Boolean
collision' loc1 loc2 = collision2 (toVertices (off loc1)) (off loc2)
  where
    off l  = l{xpos = l.xpos + l.xoffset + l.perimeter,
               ypos = l.ypos + l.yoffset + l.perimeter }

-- | No offset applying to this collision function
collision'' :: Coords -> Coords -> Boolean
collision'' loc1 loc2 = collision2 (toVertices (off loc1)) (off loc2)
  where
    off l  = l{xpos = l.xpos + l.perimeter,
               ypos = l.ypos + l.perimeter }

-- Note that offset is applied to the first location passed
collision :: Coords -> Coords -> Boolean
collision loc1 loc2  = xCollision && yCollision
   where
     xCollision = loc1.xpos + loc1.w > loc2.xpos &&
                  (loc1.xpos < loc2.xpos + loc2.w)
     yCollision = loc1.ypos + loc1.h > loc2.ypos &&
                  (loc1.ypos < loc2.ypos + loc2.h)

-- tests if any of the vertices supplied fall within the rectangle defined by
-- coordinates.
collision2 :: Array Vertex -> Coords -> Boolean
collision2 vertices loc = any (hasVertex loc) vertices

hasVertex :: Coords -> Vertex -> Boolean
hasVertex { perimeter, xpos, ypos, w, h} (Vertex x y) =
  x < xpos + w + perimeter &&
  x >= xpos - perimeter &&
  y < ypos + h + perimeter &&
  y >= ypos - perimeter
