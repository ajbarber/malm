module HeroAnims where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource)
import Image (loadImg)
import Location (dampen, toCut)
import Record as Record
import Sprite (static)
import Types (Coords, Cut(..), IsAttacking(..), Source, SpriteState, attackState, direction, foldMovement, Location(..))

file :: String
file = "assets/character.png"

load :: Aff CanvasImageSource
load = loadImg file

defaultWidth :: Number
defaultWidth = 16.0

defaultHeight :: Number
defaultHeight = 20.0

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 160.0, ypos: 62.0, w: defaultWidth, h: defaultHeight }
    source = { xpos: 0.0, ypos: 0.0, w: defaultWidth, h: defaultHeight }

baseOffset :: { xoffset :: Number, yoffset :: Number, perimeter :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0, perimeter: 4.0}

walkingCuts :: Cut Coords
walkingCuts = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 0.0,  ypos: 102.0, w: defaultWidth , h: defaultHeight }
    r = { xpos: 0.0,  ypos: 38.0, w: defaultWidth, h: defaultHeight }
    u = { xpos: 0.0,  ypos: 70.0, w: defaultWidth, h: defaultHeight }
    d = { xpos: 0.0,  ypos: 6.0, w: defaultWidth, h: defaultHeight }

attackCuts :: Number -> Number -> Cut Coords
attackCuts w h  = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 4.0,  ypos: 230.0, w: w, h: h }
    r = { xpos: 4.0,  ypos: 198.0, w: w, h: h }
    u = { xpos: 4.0,  ypos: 167.0, w: w, h: h }
    d = { xpos: 4.0,  ypos: 135.0, w: w, h: h }

cut :: Int -> SpriteState -> Source
cut frameCount sprite =
  let i = toNumber frameCount
      i' = if static sprite then 0.0 else i
      c = toCut sprite.location (foldMovement direction sprite.direction)
  in
  case attackState sprite of
    Start -> dampen (Just 31.0) (Just 13.0) i (c $ attackCuts 16.0 20.0) 4.0
    _ -> dampen Nothing Nothing i' (c walkingCuts) 4.0
