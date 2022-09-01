module Hero where

import Prelude

import Data.Array (any, foldl, foldr)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Debug (spy)
import Drawing as D
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, fillText, strokeRect)
import Image (loadImg)
import Location (coordsToDirection, dampen, isObstacle, toCut)
import Record as Record
import Sprite (action, animations, drawAnimation, health, isCollision, move, perimeter, static)
import Types (Coords, Cut(..), Direction(..), DirectionTick(..), IsAttacking(..), Location(..), Movement(..), Path(..), Source, SpriteState, State, attackState, direction, foldMovement, key, reverse, reverseTick, dest)

file :: String
file = "assets/character.png"

baseOffset :: { xoffset :: Number, yoffset :: Number, perimeter :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0, perimeter: 4.0}

defaultWidth :: Number
defaultWidth = 16.0

defaultHeight :: Number
defaultHeight = 20.0

load :: Aff CanvasImageSource
load = loadImg file

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

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 160.0, ypos: 62.0, w: defaultWidth, h: defaultHeight }
    source = { xpos: 0.0, ypos: 0.0, w: defaultWidth, h: defaultHeight }

cut :: Int -> SpriteState -> Source
cut frameCount sprite =
  let i = toNumber frameCount
      i' = if static sprite then 0.0 else i
      c = toCut sprite.location (foldMovement direction sprite.direction)
  in
  case attackState sprite of
    Start -> dampen (Just 31.0) (Just 13.0) i (c $ attackCuts 16.0 20.0) 4.0
    _ -> dampen Nothing Nothing i' (c walkingCuts) 4.0

hoistState :: State -> SpriteState -> State
hoistState state sprite = state { hero = sprite }

update :: State -> Effect State
update s = pure $ hoistState s (foldr (health isCollision) (update' s) s.npc)

damageShocks :: State -> SpriteState -> SpriteState
damageShocks s ss = foldl damageShock ss s.npc

damageShock :: SpriteState -> SpriteState -> SpriteState
damageShock hero npc = case isCollision hero npc of
  true -> hero { direction = PathMovement (Path (reverseTick curDir) 18.0 End) }
  false -> hero
  where
     curDir = fst $ coordsToDirection (dest hero.location) (dest npc.location)

update' :: State -> SpriteState
update' s = (animations s.frameCount <<< perimeter <<< damageShocks s <<< move' s <<< action) s.hero

move' :: State -> SpriteState -> SpriteState
move' s ss = move (cut s.frameCount ss) (isObstacle s) ss

draw :: State -> Effect Unit
draw state = do
  drawChar state
  for_ state.hero.animation drawAnimation

drawChar :: State -> Effect Unit
drawChar state = let
  Location srcPos newPos = state.hero.location
  in do
  fillText state.ctx (show $ state.hero.health) newPos.xpos (newPos.ypos - 5.0)
  strokeRect state.ctx $ {
    x: newPos.xpos, y: newPos.ypos,
    width: newPos.w + 2.0 * srcPos.perimeter, height: newPos.h }

  D.draw state state.hero
