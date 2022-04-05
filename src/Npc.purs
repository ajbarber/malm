module Npc where

import Prelude

import Data.Array (foldl, foldr, mapMaybe)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (foldr1)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, fillText, strokeRect)
import Image (loadImg)
import Location (collision', dampen, position, toCut)
import Location (isObstacle, translate, movement)
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
update state = pure $ state { npc = mapMaybe (update' state) state.npc }

update' :: State -> SpriteState -> Maybe SpriteState
update' state npc = let
  heroPos = dest state.hero.location
  Tuple curPos newPos' = movement npc
  static = direction (key npc.direction) == None
  i' = if static then 0.0 else toNumber state.frameCount
  cut = toCut npc.location (direction $ key npc.direction) (cuts npc)
  srcPos = dampen Nothing Nothing i' cut 2.0
  blocked = isObstacle state newPos'
  colliding = collision' curPos heroPos
  attacking = isAttacking state.hero
  newPos = case blocked of
    true -> curPos
    false -> newPos'
  npc' = npc { location = Location srcPos newPos,
               health = if colliding && attacking then npc.health - 1
                        else npc.health,
               direction = if blocked then turn <$> npc.direction
                           else npc.direction } in
  case npc.health <= 0 of
    true -> Nothing
    false -> Just npc'
    where
      turn x = DirectionTick (reverse $ direction x) (speed x)

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
