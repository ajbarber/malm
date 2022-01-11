module World where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Math (floor)
import P5.Image (image2)
import Types (AsyncState, GameState(..), Coords, dest, source)

update :: GameState -> Effect GameState
update as = pure as

draw :: GameState -> Effect Unit
draw (GameState ps as) = do
  for_ ps.tileMap \n -> do
    let src = source n.loc
    let dst = dest n.loc
    let heroPos = dest as.location
    let Tuple x y = offset heroPos dst
    image2 ps.p n.e x y dst.w dst.h
                     src.xpos src.ypos (Just src.w) (Just src.h)

offset :: Coords -> Coords -> Tuple Number Number
offset hero world = Tuple trans1 trans2
  where
    trans1 = world.xpos - hero.xoffset
    trans2 = world.ypos - hero.yoffset
