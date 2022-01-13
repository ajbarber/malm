module Hero where

import Data.Array (any, filter)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Event (direction)
import Graphics.Canvas (drawImageFull)
import Math ((%))
import Prelude (Unit, flip, pure, ($), (&&), (*), (+), (-), (/), (<), (<$>), (==), (>))
import Record as Record
import Types (AsyncState, Coords, Cut(..), Direction(..), State, Location(..), Source, dest, offsetX, offsetY, slot)

file :: String
file = "assets/character.png"

width :: Number
width = 320.0

baseOffset :: { xoffset :: Number, yoffset :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0}

cuts :: Cut Coords
cuts = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 0.0,  ypos: 96.0, w: 16.0, h: 32.0 }
    r = { xpos: 0.0,  ypos: 32.0, w: 16.0, h: 32.0 }
    u = { xpos: 0.0,  ypos: 64.0, w: 16.0, h: 32.0 }
    d = { xpos: 0.0,  ypos: 0.0, w: 16.0, h: 32.0 }

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 160.0, ypos: 62.0, w: 16.0, h: 32.0  }
    source = { xpos: 0.0,  ypos: 0.0, w: 16.0, h: 32.0 }

-- delta :: Number
-- delta = 1.0

update :: State -> Effect State
update state = do
  let i = state.frameCount
      curPos = dest state.location
      newPos' =  position (Milliseconds 1.0) state
      iNum = toNumber i
      static = (direction state.direction) == None
      i' = if static then 0.0 else iNum
      srcPos = offset i' $ toCut state
      newPos = case (isCollision state newPos') of
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
isCollision ps loc = any (\x -> collision loc (dest x.loc)) (walls ps.tileMap)
  where
    walls tm = filter _.wall tm

collision :: Coords -> Coords -> Boolean
collision loc tileLoc = xCollision && yCollision
   where
     xCollision = offsetX loc + loc.w > tileLoc.xpos &&
                  (offsetX loc < tileLoc.xpos + tileLoc.w)
     yCollision = offsetY loc + loc.h > tileLoc.ypos &&
                  (offsetY loc < tileLoc.ypos + tileLoc.h)
