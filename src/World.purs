module World where

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
      filter (\t -> let Tuple x y = offset heroPos (dest t.location) in
      x >= -perimeter && x <= 320.0 + perimeter &&
      y >= -perimeter && y <= 180.0 + perimeter) tileMap

perimeter :: Number
perimeter = 128.0

text :: State -> Effect Unit
text state = do
  setFillStyle state.ctx "black"
  setTransform state.ctx { m11: 0.5, m12: 0.0, m21: 0.0, m22:0.50, m31: 0.0, m32: 0.0}
  setFont state.ctx "36px pixel"

  --scale state.ctx { scaleX : 3.0, scaleY: 3.0 }
  traverse_ (\(Vertex x y) -> fillText state.ctx ("x:" <> show x <> "/" <> "y:" <> show y) x y) (toVertices (dest state.npc.location))
  fillText state.ctx (show state.hero.health) 300.0 8.0
  setTransform state.ctx { m11: 1.0, m12: 0.0, m21: 0.0, m22:1.0, m31: 0.0, m32: 0.0}
