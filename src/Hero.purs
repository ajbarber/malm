module Hero where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource, drawImageFull, strokeRect)
import Image (loadImg)
import Location (collision', dampen, isCollision, isObstacle, position, toCut)
import Record as Record
import Types (Action(..), Animation, AnimationType(..), Coords, Cut(..), Direction(..), Location(..), Scene(..), Source, State, dest, direction, isAttacking, key, dec)

file :: String
file = "assets/character.png"

width :: Number
width = 16.0

height :: Number
height = 20.0

baseOffset :: { xoffset :: Number, yoffset :: Number }
baseOffset = { xoffset: 0.0, yoffset: 0.0}

load :: Aff CanvasImageSource
load = loadImg file

walkingCuts :: Cut Coords
walkingCuts = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 0.0,  ypos: 102.0, w: width, h: height }
    r = { xpos: 0.0,  ypos: 38.0, w: width, h: height }
    u = { xpos: 0.0,  ypos: 70.0, w: width, h: height }
    d = { xpos: 0.0,  ypos: 6.0, w: width, h: height }

attackCuts :: Cut Coords
attackCuts = (flip Record.merge baseOffset) <$> Cut l r u d
  where
    l = { xpos: 3.0,  ypos: 144.0, w: width, h: height }
    r = { xpos: 3.0,  ypos: 204.0, w: width, h: height }
    u = { xpos: 3.0,  ypos: 174.0, w: width, h: height }
    d = { xpos: 3.0,  ypos: 134.0, w: width, h: height }

initLoc :: Location Coords
initLoc = (flip Record.merge baseOffset) <$> Location source dest
  where
    dest = { xpos: 160.0, ypos: 62.0, w: width, h: height  }
    source = { xpos: 0.0, ypos: 0.0, w: width, h: height }

cut :: State -> Source
cut state =
  let hero = state.hero
      static = direction (key hero.direction) == None
      i' = if static then 0.0 else toNumber state.frameCount in
  case isAttacking hero of
    true -> dampen i' $ toCut hero.location (direction $ key hero.direction) attackCuts
    false -> dampen i' $ toCut hero.location (direction $ key hero.direction) walkingCuts

update :: State -> Effect State
update state = do
  let hero = state.hero
      npcPos = dest state.npc.location
      heroPos = dest hero.location
      newPos' =  position state.hero
      srcPos = cut state
      newPos = case isObstacle state newPos' of
        true -> heroPos
        false -> newPos'
      scene = if hero.health < 0 then Dead 100 else state.scene
  pure $ state{ scene = scene,
                hero{ location = Location srcPos newPos,
                      health = damage (collision' newPos' npcPos) hero.health,
                      animation = updateAnimFrame state.frameCount hero.animation}}

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
drawChar state = do
  strokeRect state.ctx $ {
    x: newPos.xpos, y: newPos.ypos,
    width: newPos.w, height:  newPos.h }

  drawImageFull state.ctx state.hero.img
    srcPos.xpos
    srcPos.ypos
    srcPos.w
    srcPos.h
    newPos.xpos
    newPos.ypos
    newPos.w
    newPos.h
  where Location srcPos newPos = state.hero.location

drawAnimation :: Animation -> Effect Unit
drawAnimation anim = case anim.type_ of
  Damage -> pure unit
  Dying -> pure unit
