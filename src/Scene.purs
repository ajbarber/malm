module Scene where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Hero as Hero
import Npc as Npc
import Types (Direction(..), LoadedTileMap, Scene(..), State)
import World as World

update :: State -> Effect State
update state = case state.scene of
  Main -> World.update state
  Dead seconds -> pure state

draw :: State -> Effect Unit
draw state = case state.scene of
  Main -> World.draw state
  _ -> pure unit

init ::
  Context2D ->
  CanvasImageSource ->
  CanvasImageSource ->
  LoadedTileMap ->
  Effect State
init ctx hero npc tiles = do
  t <- liftEffect now
  pure {
    scene: Main,
    deltaTime: unInstant t,
    frameCount: 0,
    ctx: ctx,
    hero: {
      img: hero,
      direction: [],
      location: Hero.initLoc,
      animation: Nothing,
      health: 100
      },
    npc: {
      img: npc,
      direction: [ Up ],
      location: Npc.initLoc,
      animation: Nothing,
      health: 100
      },
    tileMap: tiles }