module Overworld where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Drawing as D
import Effect (Effect)
import Graphics.Canvas (fillRect, fillText, setFillStyle, setFont)
import Location (offset)
import Types (State, dest, foldMovement, key)

update :: State -> Effect State
update = pure

draw :: State -> Effect Unit
draw state = do
  let heroPos = dest state.hero.location
  setFillStyle state.ctx "#43c443"
  fillRect state.ctx { width: 420.0, height: 280.0, x:0.0, y:0.0 }
  for_ (visible heroPos state.tileMap.tiles) (D.draw state)
  text state
    where
    visible heroPos tileMap =
      filter (\t -> let { xpos, ypos} = offset heroPos (dest t.location) in
      xpos >= -perimeter && xpos <= 420.0 + perimeter &&
      ypos >= -perimeter && ypos <= 280.0 + perimeter) tileMap

perimeter :: Number
perimeter = 128.0

text :: State -> Effect Unit
text state = do
  setFillStyle state.ctx "black"
  setFont state.ctx "8px pixel"
  -- traverse_ (\(Vertex x y) -> fillText state.ctx ("x:" <> show x <> "/" <> "y:" <> show y) x y) (toVertices (dest state.npc.location))
  -- fillText state.ctx ("Perimeter :" <> (show $ (dest state.hero.location).perimeter))  20.0 8.0
  fillText state.ctx (fromMaybe "" (foldMovement show state.hero.direction))  100.0 8.0
  fillText state.ctx (show $ key state.hero.action)  100.0 8.0
  fillText state.ctx (show state.hero.health) 260.0 8.0
