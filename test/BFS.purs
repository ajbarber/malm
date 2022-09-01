module Test.BFS  where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Types (Coords)

-- gridBlock :: Number -> Number -> Coords
-- gridBlock x y = { xpos: x, ypos: y, w: 16.0, h: 16.0, xoffset: 0.0, yoffset: 0.0, perimeter: 0.0  }

-- testWalk :: Effect Unit
-- testWalk = let
--   tileMap = { walls: [],
--               xMax: src.xpos + 120.0,
--               yMax: src.ypos + 120.0,
--               xMin: src.xpos - 120.0,
--               yMin: src.ypos - 120.0 }
--   dest = gridBlock 160.0 62.0
--   src = gridBlock 94.0 136.0 in
--   launchAff_
--   do
--     liftEffect $ logShow $ (walk tileMap 16.0 src dest)
