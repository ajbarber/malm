module Scene where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Dead as Dead
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import GraphRep (toGraph)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Hero as Hero
import Npc as Npc
import Types (Direction(..), DirectionTick(..), LoadedTileMap, Movement(..), Path(..), Scene(..), State)
import World as World

scene :: State -> Scene
scene state = if (state.hero.health < 0 && state.scene == Main) then Dead 100
              else state.scene

update :: State -> Effect State
update state = case (scene state) of
    Main -> World.update state
    Dead seconds -> Dead.update state { scene = Dead seconds }

draw :: State -> Effect Unit
draw state = case state.scene of
  Main -> World.draw state
  Dead _ -> Dead.draw state

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
      direction: InputMovement mempty,
      action: mempty,
      location: Hero.initLoc,
      animation: Nothing,
      health: 100,
      width: 16.0,
      height: 20.0
      },
    npc: [{
      img: npc,
      direction: PathMovement End,
      action: mempty,
      location: Npc.initLoc,
      animation: Nothing,
      health: 100,
      width: 24.0,
      height: 28.0
      }],
    tileMap: tiles,
    graph: toGraph tiles.xMax tiles.tiles}
