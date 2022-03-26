module Hero where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, drawImageFull, strokeRect)
import Image (loadImg)
import Location (collision', dampen, isObstacle, position, toCut)
import Record as Record
import Types (Animation, AnimationType(..), Coords, Cut(..), Location(..), Scene(..), Source, SpriteState, State, dest, direction, isAttacking, key, speed)

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

static :: SpriteState -> Boolean
static ss = (speed <<< key $ ss.direction) == 0.0

cut :: State -> Source
cut state =
  let hero = state.hero
      i = toNumber state.frameCount
      i' = if static hero then 0.0 else i
      c = toCut hero.location (direction $ key hero.direction)
  in
  case isAttacking hero of
    true -> dampen (Just 31.0) (Just 13.0) i $ c (attackCuts 16.0 20.0)
    false -> dampen Nothing Nothing i' $ c walkingCuts

update :: State -> Effect State
update state = do
  let hero = state.hero
      npcPos = dest state.npc.location
      heroPos = dest hero.location
      newPos' =  position hero
      srcPos = cut state
      newPos = case (isObstacle state newPos' || static hero) of
        true -> heroPos
        false -> newPos'
      scene = if hero.health < 0 then Dead 100 else state.scene
  pure $ state{ scene = scene,
                hero{ location = Location srcPos { perimeter = inflate hero } newPos,
                      health = damage (collision' newPos' npcPos) hero.health,
                      animation = updateAnimFrame state.frameCount hero.animation}}
  where
     inflate hero = if isAttacking hero then 4.0 else 0.0

damage :: Boolean -> Int -> Int
damage collision health = case collision of
  true ->  health - 1
  false -> health

updateAnimFrame :: Int -> Maybe Animation -> Maybe Animation
updateAnimFrame fc anim = removeExpiredFrame (reduceFrame fc <$> anim)
   where
     reduceFrame n a = a { frames = a.frames - n }

removeExpiredFrame :: Maybe Animation -> Maybe Animation
removeExpiredFrame animation = case animation of
  Just anim -> if anim.frames < 0 then Nothing else animation
  Nothing -> Nothing

draw :: State -> Effect Unit
draw state = do
  drawChar state
  for_ state.hero.animation drawAnimation

drawChar :: State -> Effect Unit
drawChar state = let
  Location srcPos newPos = state.hero.location
  fullPerimeter = 2.0 * srcPos.perimeter in do
  strokeRect state.ctx $ {
    x: newPos.xpos, y: newPos.ypos,
    width: newPos.w + 2.0 * srcPos.perimeter, height: newPos.h }

  drawImageFull state.ctx state.hero.img
    srcPos.xpos
    srcPos.ypos
    (srcPos.w + fullPerimeter)
    (srcPos.h + fullPerimeter)
    (newPos.xpos + if srcPos.perimeter > 0.0 then 0.0 else baseOffset.perimeter)
    newPos.ypos
    (newPos.w + fullPerimeter)
    (newPos.h + fullPerimeter)

drawAnimation :: Animation -> Effect Unit
drawAnimation anim = case anim.type_ of
  Damage -> pure unit
  Dying -> pure unit
