module Hero where

import Data.Array (any, filter)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Event (direction)
import Graphics.Canvas (drawImageFull)
import Math ((%))
import Prelude
import Record as Record
import Types (AsyncState, Coords, Cut(..), Direction(..), State, Location(..), Source, dest, slot)

file :: String
file = "assets/character.png"

width :: Number
width = 16.0

height :: Number
height = 32.0

baseOffset :: { xoffset :: Number, yoffset :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0}

cuts :: Cut Coords
cuts = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 0.0,  ypos: 96.0, w: width, h: height }
    r = { xpos: 0.0,  ypos: 32.0, w: width, h: height }
    u = { xpos: 0.0,  ypos: 64.0, w: width, h: height }
    d = { xpos: 0.0,  ypos: 0.0, w: width, h: height }

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 160.0, ypos: 62.0, w: width, h: height  }
    source = { xpos: 0.0,  ypos: 0.0, w: width, h: height }

update :: State -> Effect State
update state = do
  let i = state.frameCount
      curPos = dest state.location
      newPos' =  position (Milliseconds 1.0) state
      iNum = toNumber i
      static = (direction state.direction) == None
      i' = if static then 0.0 else iNum
      srcPos = offset i' $ toCut state
      newPos = case (isCollision state newPos' || isBoundary state newPos') of
        true -> curPos
        false -> newPos'
  pure $ state{ location = Location srcPos newPos }

draw :: State -> Effect Unit
draw state = do
  drawImageFull state.ctx state.hero
    srcPos.xpos
    srcPos.ypos
    srcPos.w
    srcPos.h
    newPos.xpos
    newPos.ypos
    newPos.w
    newPos.h
  where Location srcPos newPos = state.location

toCut :: State -> State
toCut state = let src = slot cuts (direction state.direction) in
  state{ location = Location src (dest state.location) }

offset :: Number -> State -> Source
offset frame state = do
  src { xpos = src.xpos + (dampen frame) % 2.0 * src.w }
  where
    dampen x = x - x % 9.0
    Location src dst = state.location

position :: Milliseconds -> State -> Coords
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

offsetX :: Coords -> Number
offsetX coords = coords.xpos + coords.xoffset

offsetY :: Coords -> Number
offsetY coords = coords.ypos + coords.yoffset
