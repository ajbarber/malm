module Npc where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import GraphRep (toNode)
import Location (distance, isObstacle, side, translate)
import NpcAnims (cut)
import Path (appendStartPoint, toPath)
import SP (sp)
import Sprite (action, animations, health, isCollision', move, perimeter, turnBlocked)
import Types (Coords, DirectionTick(..), Location(..), Movement(..), Path(..), SpriteState, State, dest, isAttacking)

update :: State -> Effect State
update state = pure $ state { npc = map (update' state) state.npc }

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
      ss { direction = PathMovement (appendStartPoint l1 (toPath xMax path_)) }
  else ss

pos :: State -> SpriteState -> Coords
pos s ss = let
  Location _ newPos = ss.location in
  translate s newPos
