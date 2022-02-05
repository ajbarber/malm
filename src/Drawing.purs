module Drawing where

import Prelude

import Location (translate)
import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, drawImageFull)
import Location (offset)
import Types (Location(..), State, Coords, dest)

-- Renders a sprite. Requires the state to perform a translation
-- of coordinates based on where the hero is.
draw ::
  forall a. State ->
  { img :: CanvasImageSource, location :: Location Coords | a } ->
  Effect Unit
draw state { img, location: Location srcPos dstPos } = do
  drawImageFull state.ctx img
    srcPos.xpos
    srcPos.ypos
    srcPos.w
    srcPos.h
    dstPos'.xpos
    dstPos'.ypos
    dstPos'.w
    dstPos'.h
  where
    dstPos' = translate state dstPos
