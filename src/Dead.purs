module Dead where

import Prelude

import Effect (Effect)
import HeroAnims as HeroAnims
import Types (Scene(..), State)
import World as World

update :: State -> Effect State
update state = case state.scene of
  Main -> pure state
  Dead left -> case (left > 0) of
    true -> pure state { scene = Dead (left - 1) }
    false ->  pure state { scene = Main,
                           hero = state.hero { location = HeroAnims.initLoc,
                                               health = 100 }}

draw :: State -> Effect Unit
draw state = case state.scene of
  Main -> World.draw state
  Dead _ -> pure unit
