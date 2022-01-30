module World where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (drawImageFull, fill, fillRect, setFillStyle)
import Location (offset)
import Math (floor)
import Types (State, dest, source)

update :: State -> Effect State
update gs = pure gs

draw :: State -> Effect Unit
draw state = do
  let heroPos = dest state.hero.location
  setFillStyle state.ctx "#43c443"
  fillRect state.ctx { width: 320.0, height: 180.0, x:0.0, y:0.0 }
  for_ (visible heroPos state.tileMap.tiles) \n -> do
    let src = source n.loc
        dst = dest n.loc
        Tuple x y = offset heroPos dst
    drawImageFull state.ctx n.e src.xpos src.ypos src.w src.h
                  x y src.w src.h
    where
    visible heroPos tileMap = filter (\t -> let Tuple x y = offset heroPos (dest t.loc) in
      x >= -perimeter && x <= 320.0 + perimeter &&
      y >= -perimeter && y <= 180.0 + perimeter) tileMap

perimeter :: Number
perimeter = 128.0
