module NpcAnims where

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

-- need a map of states to cuts.
file :: String
file = "assets/npc/npcs.png"

load :: Aff CanvasImageSource
load = loadImg file

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 20.0, ypos: 62.0, w: defaultWidth, h: defaultHeight  }
    source = { xpos: 680.0,  ypos: 592.0, w: defaultWidth, h: defaultHeight }

defaultWidth :: Number
defaultWidth = 24.0

defaultHeight :: Number
defaultHeight = 28.0

baseOffset :: { xoffset :: Number, yoffset :: Number, perimeter :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0, perimeter: 0.0 }

cuts :: SpriteState -> Cut Coords
cuts ss = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 658.0,  ypos: 628.0, w: ss.width, h: ss.height }
    r = { xpos: 658.0,  ypos: 564.0, w: ss.width, h: ss.height }
    u = { xpos: 658.0,  ypos: 532.0, w: ss.width, h: ss.height }
    d = { xpos: 658.0,  ypos: 592.0, w: ss.width, h: ss.height }

cut :: Int -> SpriteState -> Source
cut frameCount sprite =
  let i = toNumber frameCount
      i' = if static sprite then 0.0 else i
      c = toCut sprite.location (foldMovement direction sprite.direction)
  in
  dampen Nothing Nothing i' (c $ cuts sprite) 2.0
