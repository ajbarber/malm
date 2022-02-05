module World where

import Prelude

import Control.Applicative (($>))
import Data.Array (head)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (negateDuration)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Event (keys)
import Hero as Hero
import Npc as Npc
import Overworld as Overworld
import Types (State, AsyncState)

step :: Maybe AsyncState -> State -> Effect State
step event state = do
  t <- liftEffect now
  dir <- case (head $ fromMaybe [] event) of
    Just ev -> keys ev state.hero.direction
    Nothing -> pure state.hero.direction

  state' <- Overworld.update (stepSt state t dir) >>= Hero.update >>= Npc.update
  Overworld.draw state' *>  Npc.draw state' *>  Hero.draw state'
  pure state'
  where
    stepSt st time dir = st {
      deltaTime = unInstant time <> negateDuration (st.deltaTime),
      frameCount = st.frameCount + 1,
      hero { direction = dir }
      }
