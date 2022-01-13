module World where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (drawImageFull)
import Math (floor)
import Types (Coords, GameState(..), dest, source)

update :: GameState -> Effect GameState
update gs = pure gs

draw :: GameState -> Effect Unit
draw (GameState ps as) = do
  let heroPos = dest as.location
  for_ (visible heroPos ps.tileMap) \n -> do
    let src = source n.loc
        dst = dest n.loc
        Tuple x y = offset heroPos dst
    drawImageFull ps.ctx n.e src.xpos src.ypos src.w src.h
                  x y src.w src.h
             where
    visible heroPos tileMap = filter (\t -> let Tuple x y = offset heroPos (dest t.loc) in
      x >= -perimeter && x <= 320.0 + perimeter &&
      y >= -perimeter && y <= 180.0 + perimeter) tileMap

perimeter :: Number
perimeter = 128.0

offset :: Coords -> Coords -> Tuple Number Number
offset hero world = Tuple trans1 trans2
  where
    trans1 = world.xpos  - (floor hero.xoffset)
    trans2 = world.ypos  - (floor hero.yoffset)
