module Npc where

import Prelude

import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Event (direction)
import Graphics.Canvas (CanvasImageSource, Rectangle, drawImageFull, strokeRect)
import Image (loadImg)
import Location (dampen, isBlocked, isBoundary, isObstacle, offset, position, toCut)
import Math (floor)
import Record as Record
import Types (Coords, Cut(..), Direction(..), Location(..), State, dest, reverse)

file :: String
file = "assets/npc/npcs.png"

width :: Number
width = 24.0

height :: Number
height = 28.0

baseOffset :: { xoffset :: Number, yoffset :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0}

load :: Aff CanvasImageSource
load = loadImg file

cuts :: Cut Coords
cuts = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 656.0,  ypos: 626.0, w: width, h: height }
    r = { xpos: 656.0,  ypos: 562.0, w: width, h: height }
    u = { xpos: 656.0,  ypos: 530.0, w: width, h: height }
    d = { xpos: 656.0,  ypos: 592.0, w: width, h: height }

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 60.0, ypos: 62.0, w: floor width, h: floor height  }
    source = { xpos: 680.0,  ypos: 592.0, w: width, h: height }

update :: State -> Effect State
update state = do
  let i = state.frameCount
      npc = state.npc
      curPos = dest npc.location
      newPos' =  position (Milliseconds 1.0) state.npc
      iNum = toNumber i
      static = (direction npc.direction) == None
      i' = if static then 0.0 else iNum
      srcPos = dampen i' $ toCut npc.location npc.direction cuts
      blocked = isBlocked state newPos'
      newPos = case blocked of
        true -> curPos
        false -> newPos'
  pure $ state{ npc{location = Location srcPos newPos,
                    direction = if blocked then
                                  reverse <$> state.npc.direction
                                else
                                  state.npc.direction }}

draw :: State -> Effect Unit
draw state = do
  strokeRect state.ctx $ {
    x: newPos.xpos, y: newPos.ypos,
    width: newPos.w, height:  newPos.h }

  D.draw state state.npc
  where
    Location _ newPos = state.npc.location
