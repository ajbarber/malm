module Npc where

import Prelude

import Data.Array (findIndex)
import Data.Foldable (for_, sequence_, traverse_, foldl)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy, trace)
import Debugging (drawLPath)
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graph (LPath(..))
import GraphRep (fromNode, toNode)
import Graphics.Canvas (CanvasImageSource, closePath, fillPath, fillRect, fillText, lineTo, moveTo, stroke, strokePath, strokeRect)
import Image (loadImg)
import Location (collision', dampen, distance, isObstacle, position, side, snap, toCut, translate)
import Math ((%))
import Path (appendStartPoint, toPath)
import Record as Record
import SP (sp)
import Sprite (action, animations, health, isCollision', move, perimeter, static, turnBlocked)
import Types (Coords, Cut(..), DirectionTick(..), Location(..), Movement(..), Path(..), Source, SpriteState, State, dest, direction, foldMovement, isAttacking, key, toCoords)

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
    dest = { xpos: 20.0, ypos: 62.0, w: defaultWidth, h: defaultHeight  }
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

damageShock :: SpriteState -> SpriteState -> SpriteState
damageShock npc hero = case collisionFunc npc hero of
  true -> npc { direction = PathMovement (Path (DirectionTick curDir 2.0) 36.0 End) }
  false -> npc
  where
     curDir = side (dest npc.location) (dest hero.location)

turnAround :: State -> SpriteState -> SpriteState
turnAround state = turnBlocked state (isObstacle state)

update' :: State -> SpriteState -> SpriteState
update' s = (animations s.frameCount
             <<< health collisionFunc s.hero
             <<< action
             <<< flip damageShock s.hero
             <<< path s
             <<< perimeter
             <<< move' s)

path :: State -> SpriteState -> SpriteState
path s ss = let
  l1 = dest ss.location
  l2 = dest s.hero.location
  in
  if (distance l1 l2 <= 200.0 && ss.direction == PathMovement End) then
      let xMax = s.tileMap.xMax
          n1 = toNode xMax (l1.xpos /\ l1.ypos)
          n2 = toNode xMax ((l2.xpos + l2.xoffset) /\ (l2.ypos + l2.yoffset))
          path_ = sp n2 n1 s.graph in
      ss { direction = spy (show path_  <> "///" <> (show $ appendStartPoint l1 (toPath xMax path_))) PathMovement (appendStartPoint l1 (toPath xMax path_)) }
  else spy ("distance npc" <> ((show $ distance l1 l2)) ) ss

pos :: State -> SpriteState -> Coords
pos s ss = let
  Location _ newPos = ss.location in
  translate s newPos

draw :: State -> Effect Unit
draw state = sequence_ $ map (drawSingle state) state.npc

drawSingle :: State -> SpriteState -> Effect Unit
drawSingle state npc = let
  newPos' = pos state npc
  sourceLoc = dest npc.location
  -- destLoc = dest state.hero.location
  -- path_ = sp (toNode state.tileMap.xMax (sourceLoc.xpos /\ sourceLoc.ypos))
  --            (toNode state.tileMap.xMax ((destLoc.xpos + destLoc.xoffset) /\ (destLoc.ypos + destLoc.yoffset))) state.graph
  tempNode = toNode state.tileMap.xMax (sourceLoc.xpos /\ sourceLoc.ypos)
  (tempX /\ tempY) = fromNode state.tileMap.xMax tempNode
  in do
  fillRect state.ctx {x:tempX, y: tempY, width: 10.0, height: 5.0}
  fillText state.ctx (show npc.health) newPos'.xpos (newPos'.ypos - 5.0)
  fillText state.ctx ("xMax:" <> show state.tileMap.xMax) (newPos'.xpos + 70.0) (newPos'.ypos - 5.0)
  fillText state.ctx (if (isObstacle state newPos') then "Blocked" else "NA") (newPos'.xpos + 70.0) (newPos'.ypos - 10.0)
  strokeRect state.ctx { x: newPos'.xpos, y: newPos'.ypos,
                         width: newPos'.w, height: newPos'.h }
  -- drawLPath state path_
  D.draw state npc

toXY :: Number -> LPath Int -> List (Tuple Number Number)
toXY max (LP { unLPath: arr }) = map (\(f /\ s) -> fromNode max f) arr
