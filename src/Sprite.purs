module Sprite where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Location (collision', distance, isObstacle, movement, position)
import Types (Action(..), Animation, AnimationType(..), Coords, DirectionTick(..), IsAttacking(..), Location(..), Movement(..), Path(..), Source, SpriteState, State, attackState, dest, direction, foldMovement, key, reverse, speed)

static :: SpriteState -> Boolean
static ss = foldMovement speed ss.direction == Just 0.0

perimeter :: SpriteState -> SpriteState
perimeter sprite =
  let perimeter' = inflate sprite in
  sprite { location = (\x -> x{ perimeter = perimeter' }) <$> sprite.location }

blocked :: State -> SpriteState -> Boolean
blocked state sprite = (isObstacle state (dest sprite.location) || static sprite)

move :: Source -> (Coords -> Boolean) -> SpriteState -> SpriteState
move cut f sprite = let
  oldPos = dest sprite.location
  newPos = position sprite in
  sprite { location = Location cut (if (f newPos || static sprite) then oldPos
                                    else newPos),
           direction = updatePath oldPos newPos sprite.direction }

-- Returns a new path subtracting the distance travelled on the current path leg
-- If we have travelled all the distance on that leg, remove the node
updatePath :: forall a. Coords -> Coords -> Movement a -> Movement a
updatePath old new (PathMovement (Path a d rest)) = newPath (Path a d rest) (d - (distance old new))
  where
    newPath (Path p _ r) dst = if dst > 0.0 then PathMovement (Path p dst r) else PathMovement r
    newPath End _ = PathMovement End
updatePath _ _ i = i

animations :: Int -> SpriteState -> SpriteState
animations frame sprite = sprite { animation = updateAnimFrame frame sprite.animation }

action :: SpriteState -> SpriteState
action sprite = sprite  { action = tickAction <$> sprite.action }

-- | attacking inflates the size of sprite rectangle, we need to draw the weapon
inflate :: SpriteState -> Number
inflate sprite = if attackState sprite == Start then 4.0 else 0.0

-- | engagements with other sprites
health :: (SpriteState -> SpriteState -> Boolean) ->
          SpriteState ->
          SpriteState ->
          SpriteState
health f them us = us{ health = damage (f us them) us.health }

-- | ignore perimeter inflation in collision detection
isCollision :: SpriteState -> SpriteState -> Boolean
isCollision us them = collision' (position' us) (position' them)

-- | include perimeter inflation in collision detection
isCollision' :: SpriteState -> SpriteState -> Boolean
isCollision' us them = collision' (dest us.location) (dest them.location)

uninflate :: Coords -> Coords
uninflate coords = coords { perimeter = 0.0 }

position' :: SpriteState -> Coords
position' s = uninflate $ dest s.location

damage :: Boolean -> Int -> Int
damage collision h = case collision of
  true ->  h - 1
  false -> h

turnBlocked :: (Coords -> Boolean) -> SpriteState -> SpriteState
turnBlocked f s = let newPos = movement s in
  s { direction = if (f newPos) then turn <$> s.direction
                  else s.direction }

turn :: DirectionTick -> DirectionTick
turn x = DirectionTick (reverse $ direction x) (speed x)

tickAction :: Action -> Action
tickAction Default = Default
tickAction (Attacking 0.0) = Default
tickAction (Attacking x) = Attacking (x - 1.0)

updateAnimFrame :: Int -> Maybe Animation -> Maybe Animation
updateAnimFrame fc anim = removeExpiredFrame (reduceFrame fc <$> anim)
   where
     reduceFrame n a = a { frames = a.frames - n }

removeExpiredFrame :: Maybe Animation -> Maybe Animation
removeExpiredFrame animation = case animation of
  Just anim -> if anim.frames < 0 then Nothing else animation
  Nothing -> Nothing

drawAnimation :: Animation -> Effect Unit
drawAnimation anim = case anim.type_ of
  Damage -> pure unit
  Dying -> pure unit
