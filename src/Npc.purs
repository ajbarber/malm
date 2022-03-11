module Npc where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, strokeRect)
import Image (loadImg)
import Location (dampen, position, toCut)
import Location (isObstacle, translate)
import Math (floor)
import Record as Record
import Types (Coords, Cut(..), Direction(..), DirectionTick(..), Location(..), State, dest, direction, frames, key, reverse)

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
    l = { xpos: 658.0,  ypos: 628.0, w: width, h: height }
    r = { xpos: 658.0,  ypos: 564.0, w: width, h: height }
    u = { xpos: 658.0,  ypos: 532.0, w: width, h: height }
    d = { xpos: 658.0,  ypos: 592.0, w: width, h: height }

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 320.0, ypos: 102.0, w: width, h: height  }
    source = { xpos: 680.0,  ypos: 592.0, w: width, h: height }

update :: State -> Effect State
update state = do
  let npc = state.npc
      curPos = dest npc.location
      newPos' = position state.npc
      static = direction (key npc.direction) == None
      i' = if static then 0.0 else toNumber state.frameCount
      srcPos = dampen i' $ toCut npc.location (direction $ key npc.direction) cuts
      blocked = isObstacle state newPos'
      newPos = case blocked of
        true -> curPos
        false -> newPos'
  pure $ state{ npc{location = Location srcPos newPos,
                    direction = if blocked then
                                  (\x -> DirectionTick (reverse $ direction x) (frames x)) <$> npc.direction
                                else
                                  npc.direction }}

draw :: State -> Effect Unit
draw state = do
  strokeRect state.ctx $ {
    x: newPos'.xpos, y: newPos'.ypos,
    width: newPos'.w, height: newPos'.h }

  D.draw state state.npc
  where
    Location _ newPos = state.npc.location
    newPos' = translate state newPos
