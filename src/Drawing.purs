module Drawing where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, drawImageFull)
import Location (translate)
import Types (Coords, Location(..), State)

-- Renders a sprite. Requires the state to perform a translation
-- of coordinates based on where the hero is.
draw ::
  forall a. State ->
  { img :: CanvasImageSource, location :: Location Coords | a } ->
  Effect Unit
draw state { img, location: Location srcPos dstPos } = let
  fullPerimeter = 2.0 * srcPos.perimeter in do
  drawImageFull state.ctx img
    srcPos.xpos
    srcPos.ypos
    (srcPos.w + fullPerimeter)
    (srcPos.h + fullPerimeter)
    dstPos'.xpos
    dstPos'.ypos
    (dstPos'.w + fullPerimeter)
    (dstPos'.h + fullPerimeter)
  where
    dstPos' = translate state dstPos
