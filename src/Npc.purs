module Npc
  where

import Prelude

import Data.Array (findIndex)
import Data.Foldable (for_, sequence_, traverse_, foldl)
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graph (LPath(..))
import GraphRep (fromNode, toNode)
import Graphics.Canvas (CanvasImageSource, closePath, fillPath, fillRect, fillText, lineTo, moveTo, stroke, strokePath, strokeRect)
import Image (loadImg)
import Location (collision', dampen, distance, isObstacle, toCut, translate)
import Math ((%))
import Record as Record
import SP (sp)
import Sprite (action, animations, health, isCollision', move, perimeter, static, turnBlocked)
import Types (Coords, Cut(..), Location(..), Movement(..), Path(..), Source, SpriteState, State, dest, direction, foldMovement, isAttacking, key)

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

turnAround :: State -> SpriteState -> SpriteState
turnAround = turnBlocked <<< isObstacle

update' :: State -> SpriteState -> SpriteState
update' s = (animations s.frameCount
             <<< health collisionFunc s.hero
             <<< action
             <<< path s
             <<< perimeter
             <<< move' s)

path :: State -> SpriteState -> SpriteState
path s ss = let
  sourceLoc = pos s ss
  -- path = sp ( curGraphNode s ss) curGraphNode (s s.hero) s.graph
  destLoc = dest s.hero.location
  in
  case (toNumber s.frameCount) % 100.0 of
   0.0 -> ss { direction = PathMovement End}
   _ -> ss
  -- if ss.direction == PathMovement End &&
  --    ((toNumber s.frameCount) % 100.0) == 0.0
  --    then ss { direction = PathMovement End }
  --   else ss
  where
    tileMap' sourceLoc = s.tileMap { xMin = sourceLoc.xpos - 120.0,
                                     xMax = sourceLoc.xpos + 120.0,
                                     yMin = sourceLoc.ypos - 120.0,
                                     yMax = sourceLoc.ypos + 120.0 }

pos :: State -> SpriteState -> Coords
pos s ss = let
  Location _ newPos = ss.location in
  translate s newPos

draw :: State -> Effect Unit
draw state = sequence_ $ map (drawSingle state) state.npc

drawSingle :: State -> SpriteState -> Effect Unit
drawSingle state npc = let
  newPos' = pos state npc
  sourceLoc = pos state npc
  -- path = sp ( curGraphNode s ss) curGraphNode (s s.hero) s.graph
  destLoc = dest state.hero.location
  path_ =  sp (toNode state.tileMap.xMax (sourceLoc.xpos /\ sourceLoc.ypos))
            (toNode state.tileMap.xMax (destLoc.xpos /\ destLoc.ypos)) state.graph
  tempNode = toNode state.tileMap.xMax (sourceLoc.xpos /\ sourceLoc.ypos)
  (tempX /\ tempY) = fromNode state.tileMap.xMax tempNode
  in do
  fillRect state.ctx {x:tempX, y: tempY, width: 10.0, height: 5.0}
  fillText state.ctx (show $ "(" <> show newPos'.xpos <> "," <> show newPos'.ypos <> ")") newPos'.xpos (newPos'.ypos - 5.0)
  fillText state.ctx ("xMax:" <> show state.tileMap.xMax) (newPos'.xpos + 70.0) (newPos'.ypos - 5.0)
  strokeRect state.ctx { x: newPos'.xpos, y: newPos'.ypos,
                         width: newPos'.w, height: newPos'.h }
  drawLPath state path_
  D.draw state npc

drawLPath :: State -> LPath Int -> Effect Unit
drawLPath state (LP { unLPath: arr}) = void $ foldl (\prev (v1 /\ l1) -> do
  (v0 /\ l0) <- prev
  fillPath state.ctx $ do
    let (x0 /\ y0) = fromNode state.tileMap.xMax v0
    let (x1 /\ y1) = fromNode state.tileMap.xMax v1
    if (v0 > 0) then do
      moveTo state.ctx x0 y0
      lineTo state.ctx x1 y1
      stroke state.ctx
    else pure unit
  pure (v1 /\ l1))
  (pure (0 /\ 0))
  arr

toXY :: Number -> LPath Int -> List (Tuple Number Number)
toXY max (LP { unLPath: arr }) = map (\(f /\ s) -> fromNode max f) arr
