module World where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Math (floor, (%))
import P5.Environment (frameCount)
import P5.Image (image2)
import P5.Math (createVector)
import P5.Rendering (createGraphics)
import P5.Transform (translate)
import P5.Types (ElementOrImage(..))
import Types (Coords, GameState(..), dest, source)


update :: GameState -> Effect GameState
update gs = pure gs

draw :: GameState -> Effect Unit
draw (GameState ps as) = do
  i <- frameCount ps.p
  let heroPos = dest as.location
  for_ (visible heroPos ps.tileMap) \n -> do
    let src = source n.loc
        dst = dest n.loc
        Tuple x y = offset heroPos dst

    image2 ps.p n.e x y src.w src.h
          src.xpos src.ypos (Just src.w) (Just src.h)
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
