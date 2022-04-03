module Npc where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, strokeRect)
import Image (loadImg)
import Location (collision', dampen, position, toCut)
import Location (isObstacle, translate)
import Record as Record
import Types (Coords, Cut(..), Direction(..), DirectionTick(..), Location(..), SpriteState, State, dest, direction, isAttacking, key, reverse, speed)

file :: String
file = "assets/npc/npcs.png"

baseOffset :: { xoffset :: Number, yoffset :: Number, perimeter :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0, perimeter: 0.0 }

defaultWidth :: Number
defaultWidth = 24.0

defaultHeight :: Number
defaultHeight = 28.0

load :: Aff CanvasImageSource
load = loadImg file

cuts :: SpriteState -> Cut Coords
cuts ss = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 658.0,  ypos: 628.0, w: ss.width, h: ss.height }
    r = { xpos: 658.0,  ypos: 564.0, w: ss.width, h: ss.height }
    u = { xpos: 658.0,  ypos: 532.0, w: ss.width, h: ss.height }
    d = { xpos: 658.0,  ypos: 592.0, w: ss.width, h: ss.height }

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 320.0, ypos: 102.0, w: defaultWidth, h: defaultHeight  }
    source = { xpos: 680.0,  ypos: 592.0, w: defaultWidth, h: defaultHeight }

update :: State -> Effect State
update state = do
  let npc = state.npc
      hero = state.hero
      curPos = dest npc.location
      heroPos = dest hero.location
      newPos' = position state.npc
      static = direction (key npc.direction) == None || npc.health == 0
      i' = if static then 0.0 else toNumber state.frameCount
      cut = toCut npc.location (direction $ key npc.direction) (cuts npc)
      srcPos = dampen Nothing Nothing i' cut 2.0
      blocked = isObstacle state newPos'
      newPos = case blocked of
        true -> curPos
        false -> newPos'
  pure $ state{ npc{ location = Location srcPos newPos,
                     health = if collision' curPos heroPos && isAttacking hero
                              then 0
                              else npc.health,
                     direction = if blocked then turn <$> npc.direction
                                 else npc.direction }}
    where
      turn x = DirectionTick (reverse $ direction x) (speed x)

draw :: State -> Effect Unit
draw state = do
  strokeRect state.ctx $ {
    x: newPos'.xpos, y: newPos'.ypos,
    width: newPos'.w, height: newPos'.h }

  D.draw state state.npc
  where
    Location _ newPos = state.npc.location
    newPos' = translate state newPos
