module World where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_, sequence_)
import Data.Time.Duration (negateDuration)
import Drawing as D
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Hero as Hero
import Npc as Npc
import Overworld as Overworld
import Sprite (drawAnimation)
import Types (State)

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
draw state = Overworld.draw state *>
             sequence_ (map (D.draw state) state.npc) *>
             D.draw state state.hero *>
             for_ state.hero.animation drawAnimation
