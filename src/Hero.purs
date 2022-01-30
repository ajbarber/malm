module Hero where

import Prelude

import Data.Array (any, filter)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Event (direction)
import Graphics.Canvas (CanvasImageSource, drawImageFull)
import Image (loadImg)
import Location (dampen, toCut, isCollision, isBoundary, position)
import Math ((%))
import Record as Record
import Types (AsyncState, Coords, Cut(..), Direction(..), Location(..), Source, State, SpriteState, dest, slot)

file :: String
file = "assets/character.png"

width :: Number
width = 16.0

height :: Number
height = 32.0

baseOffset :: { xoffset :: Number, yoffset :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0}

load :: Aff CanvasImageSource
load = loadImg file

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
      hero = state.hero
      curPos = dest hero.location
      newPos' =  position (Milliseconds 1.0) state.hero
      iNum = toNumber i
      static = (direction hero.direction) == None
      i' = if static then 0.0 else iNum
      srcPos = dampen i' $ toCut hero.location hero.direction cuts
      newPos = case (isCollision state newPos' || isBoundary state newPos') of
        true -> curPos
        false -> newPos'
  pure $ state{ hero{ location = Location srcPos newPos }}

draw :: State -> Effect Unit
draw state = do
  drawImageFull state.ctx state.hero.img
    srcPos.xpos
    srcPos.ypos
    srcPos.w
    srcPos.h
    newPos.xpos
    newPos.ypos
    newPos.w
    newPos.h
  where Location srcPos newPos = state.hero.location
