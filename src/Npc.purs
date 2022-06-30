module Npc where

import Prelude

import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, fillText, strokeRect)
import Image (loadImg)
import Location (dampen, isObstacle, toCut, translate)
import Record as Record
import Sprite (action, animations, health, isCollision', move, perimeter, static, turnBlocked)
import Types (Coords, Cut(..), Location(..), Source, SpriteState, State, direction, foldMovement, isAttacking, key)

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
update state = pure $ state { npc = map (update' state) state.npc }

cut :: Int -> SpriteState -> Source
cut frameCount sprite =
  let i = toNumber frameCount
      i' = if static sprite then 0.0 else i
      c = toCut sprite.location (foldMovement direction sprite.direction)
  in
  dampen Nothing Nothing i' (c $ cuts sprite) 2.0

move' :: State -> SpriteState -> SpriteState
move' s ss = move (cut s.frameCount ss) (isObstacle s) ss

collisionFunc :: SpriteState -> SpriteState -> Boolean
collisionFunc us them = isCollision' us them && isAttacking them

turnAround :: State -> SpriteState -> SpriteState
turnAround = turnBlocked <<< isObstacle

update' :: State -> SpriteState -> SpriteState
update' s = (animations s.frameCount
             <<< health collisionFunc s.hero
             <<< action
             <<< perimeter
             <<< move' s)

draw :: State -> Effect Unit
draw state = sequence_ $ map (drawSingle state) state.npc

drawSingle :: State -> SpriteState -> Effect Unit
drawSingle state npc = do
  fillText state.ctx (show $ npc.health) newPos'.xpos (newPos'.ypos - 5.0)
  strokeRect state.ctx { x: newPos'.xpos, y: newPos'.ypos,
                         width: newPos'.w, height: newPos'.h }
  D.draw state npc
  where
    Location _ newPos = npc.location
    newPos' = translate state newPos
