module Debugging where

import Prelude

import Data.List (foldl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Graph (LPath(..))
import GraphRep (fromNode, toNode)
import Graphics.Canvas (fillPath, fillRect, fillText, lineTo, moveTo, stroke, strokeRect)
import Location (isObstacle, translatedPos)
import Types (Location(..), State, SpriteState, dest)

drawLPath :: State -> LPath Int -> Effect Unit
drawLPath state (LP { unLPath: arr}) = void $ foldl (\prev (v1 /\ l1) -> do
  (v0 /\ l0) <- prev
  fillPath state.ctx $ do
    let (x0 /\ y0) = fromNode state.tileMap.xMax v0
    let (x1 /\ y1) = fromNode state.tileMap.xMax v1
    if (v0 > 0) then do
      moveTo state.ctx (offX x0) (offY y0)
      lineTo state.ctx (offX x1) (offY y1)
      stroke state.ctx
    else pure unit
  pure (v1 /\ l1))
  (pure (0 /\ 0))
  arr
  where
    offX x = x - (dest state.hero.location).xoffset
    offY y = y - (dest state.hero.location).yoffset

drawChar :: State -> Effect Unit
drawChar state = let
  Location srcPos newPos = state.hero.location
  in do
  fillText state.ctx (show $ state.hero.health) newPos.xpos (newPos.ypos - 5.0)
  strokeRect state.ctx $ {
    x: newPos.xpos, y: newPos.ypos,
    width: newPos.w + 2.0 * srcPos.perimeter, height: newPos.h }

drawSingle :: State -> SpriteState -> Effect Unit
drawSingle state npc = let
  newPos' = translatedPos state npc
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
