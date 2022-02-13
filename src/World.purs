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
import Graphics.Canvas (fillText, setFont)
import Hero as Hero
import Npc as Npc
import Overworld as Overworld
import Types (State, AsyncState)

update :: State -> Effect State
update state = do
  t <- liftEffect now
  state' <- Overworld.update (stepSt state t) >>= Hero.update >>= Npc.update
  pure state'
  where
    stepSt st time = st {
      deltaTime = unInstant time <> negateDuration (st.deltaTime),
      frameCount = st.frameCount + 1
    }

draw :: State -> Effect Unit
draw state = Overworld.draw state *>  Npc.draw state *>  Hero.draw state
