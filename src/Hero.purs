module Hero where

import Data.Array (any, filter)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Event (direction)
import Math ((%))
import P5.Environment (frameCount)
import P5.Image (image2, loadImage)
import P5.Types (Image, P5, ElementOrImage(..))
import Prelude (bind, discard, pure, ($), (&&), (*), (+), (-), (<), (==), (>))
import Types (AsyncState, Coords, Cut(..), Direction(..), GameState(..), Location(..), PreloadState, Source, slot, dest)

file :: String
file = "assets/character.png"

width :: Number
width = 320.0

cuts :: Cut
cuts = Cut l r u d
  where
    l = { xpos: 0.0,  ypos: 96.0, w: 16.0, h: 32.0 }
    r = { xpos: 0.0,  ypos: 32.0, w: 16.0, h: 32.0 }
    u = { xpos: 0.0,  ypos: 64.0, w: 16.0, h: 32.0 }
    d = { xpos: 0.0,  ypos: 0.0, w: 16.0, h: 32.0 }

img :: P5 -> Image
img p = loadImage p file Nothing Nothing

initLoc :: Location
initLoc = Location source dest
  where
    dest = { xpos: 0.0, ypos: 0.0, w: 16.0, h: 32.0 }
    source = { xpos: 0.0,  ypos: 0.0, w: 16.0, h: 32.0 }

delta :: Number
delta = 1.0

shouldDraw :: Int -> Boolean
shouldDraw frame = toNumber frame % 2.0 == 0.0

draw :: GameState -> Effect AsyncState
draw (GameState ps as) = do
  i <- frameCount ps.p
  let curPos = dest as.location
  let newPos' = if (shouldDraw i) then position as else curPos
  let iNum = toNumber i
  let static = (direction as.event) == None
  let i' = if static then 0.0 else iNum
  let srcPos = offset i' $ toCut as
  let newPos = case (isCollision ps newPos') of
        true -> curPos
        false -> newPos'
  image2 ps.p (ElementOrImageImage $ ps.hero)
    newPos.xpos
    newPos.ypos
    newPos.w
    newPos.h
    srcPos.xpos
    srcPos.ypos
    (Just srcPos.w)
    (Just srcPos.h)
  pure as { location = Location srcPos newPos }

toCut :: AsyncState -> AsyncState
toCut as = let src = slot cuts (direction as.event) in
  as { location = Location src (dest as.location) }

offset :: Number -> AsyncState -> Source
offset frame as = do
  src { xpos = src.xpos + (dampen frame) % 4.0 * src.w }
  where
    dampen x = x - x % 9.0
    Location src dst = as.location

position :: AsyncState -> Coords
position st = let coords = dest st.location in
  case (direction st.event) of
    Left -> coords { xpos = coords.xpos - delta }
    Right -> coords { xpos = coords.xpos + delta }
    Up -> coords { ypos = coords.ypos - delta }
    Down -> coords { ypos = coords.ypos + delta }
    None -> coords

isCollision :: PreloadState -> Coords -> Boolean
isCollision ps loc = any (\x -> collision loc (dest x.loc)) (walls ps.tileMap)
  where
    walls tm = filter _.wall tm

collision :: Coords -> Coords -> Boolean
collision loc tileLoc = xCollision && yCollision
   where
     xCollision = loc.xpos + loc.w > tileLoc.xpos &&
                  (loc.xpos < tileLoc.xpos + tileLoc.w)
     yCollision = loc.ypos + loc.h > tileLoc.ypos &&
                  (loc.ypos < tileLoc.ypos + tileLoc.h)
