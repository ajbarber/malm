module Scene where

import Prelude

import Control.Monad.RWS (state)
import Effect (Effect)
import Types (AsyncState, Scene(..), State)
import World as World

update :: State -> Effect State
update state = case state.scene of
  Main -> World.update state
  Dead seconds -> pure state

draw :: State -> Effect Unit
draw state = case state.scene of
  Main -> World.draw state
  _ -> pure unit
