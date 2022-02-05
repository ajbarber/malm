module Overworld where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (fillRect, fillText, setFillStyle, setFont, setTransform)
import Drawing as D
import Location (offset)
import Types (State, Vertex(..), dest, toVertices)

update :: State -> Effect State
update gs = pure gs

draw :: State -> Effect Unit
draw state = do
  let heroPos = dest state.hero.location
  setFillStyle state.ctx "#43c443"
  fillRect state.ctx { width: 320.0, height: 180.0, x:0.0, y:0.0 }
  for_ (visible heroPos state.tileMap.tiles) \n -> D.draw state n
  text state
    where
    visible heroPos tileMap =
      filter (\t -> let { xpos, ypos} = offset heroPos (dest t.location) in
      xpos >= -perimeter && xpos <= 320.0 + perimeter &&
      ypos >= -perimeter && ypos <= 180.0 + perimeter) tileMap

perimeter :: Number
perimeter = 128.0

text :: State -> Effect Unit
text state = do
  setFillStyle state.ctx "black"
  setFont state.ctx "8px pixel"
  --scale state.ctx { scaleX : 3.0, scaleY: 3.0 }
  traverse_ (\(Vertex x y) -> fillText state.ctx ("x:" <> show x <> "/" <> "y:" <> show y) x y) (toVertices (dest state.npc.location))
  fillText state.ctx (show state.hero.health) 260.0 8.0
