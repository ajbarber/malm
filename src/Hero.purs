module Hero where

import Prelude

import Data.Array (foldl, foldr)
import Effect (Effect)
import HeroAnims (cut)
import Location (isObstacle, side)
import Sprite (action, animations, health, isCollision, move, perimeter)
import Types (DirectionTick(..), Movement(..), Path(..), SpriteState, State, dest)

hoistState :: State -> SpriteState -> State
hoistState state sprite = state { hero = sprite }

update :: State -> Effect State
update s = pure $ hoistState s (foldr (health isCollision) (update' s) s.npc)

damageShocks :: State -> SpriteState -> SpriteState
damageShocks s ss = foldl damageShock ss s.npc

damageShock :: SpriteState -> SpriteState -> SpriteState
damageShock hero npc = case isCollision hero npc of
  true -> hero { direction = PathMovement (Path (DirectionTick curDir 2.0) 28.0 End) }
  false -> hero
  where
     curDir = side (dest hero.location) (dest npc.location)

update' :: State -> SpriteState
update' s = (animations s.frameCount
             <<< perimeter
             <<< damageShocks s
             <<< move' s
             <<< action) s.hero

move' :: State -> SpriteState -> SpriteState
move' s ss = move (cut s.frameCount ss) (isObstacle s) ss
