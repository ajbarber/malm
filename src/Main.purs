module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (tryRead) as EVar
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (new) as AVar
import Effect.Class (liftEffect)
import Effect.Now (now)
import Event (handleEvent)
import Graphics.Canvas (CanvasImageSource, getCanvasElementById, getContext2D)
import Hero as Hero
import Npc as Npc
import TileMap (loadedTileMap)
import Types (AsyncState, Direction(..), EventType(..), LoadedTileMap, State)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import World as World

main :: Effect Unit
main = launchAff_ mainA

mainA :: Aff Unit
mainA = do
  aVar <- AVar.new []
  heroImg <- Hero.load
  npcImg <- Npc.load
  tileMap <- loadedTileMap
  liftEffect $ mainEffect aVar heroImg npcImg tileMap

mainEffect ::
  AVar AsyncState ->
  CanvasImageSource ->
  CanvasImageSource ->
  LoadedTileMap ->
  Effect Unit
mainEffect aVar hero npc tiles = do
  id <- getCanvasElementById "main"
  for_ id \id' -> do
    ctx <- getContext2D id'
    t <- liftEffect now
    runReaderT (mainS aVar) {
      deltaTime: unInstant t,
      frameCount: 0,
      ctx: ctx,
      modal: Nothing,
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

preload :: forall m. MonadAsk State m => m State
preload = ask

step :: Window -> AVar AsyncState -> State -> Effect Unit
step w aVar state = do
  event <- EVar.tryRead aVar
  state' <- World.step event state
  void $ flip requestAnimationFrame w (void $ step w aVar state')

mainS :: AVar AsyncState -> ReaderT State Effect Unit
mainS aVar = do
  ps <- preload
  liftEffect do
    w <- window

    keyDownHandler <- handleEvent KeyDown aVar
    keyUpHandler <- handleEvent KeyUp aVar
    windowTarget <- map Window.toEventTarget window

    addEventListener keydown keyDownHandler false windowTarget
    addEventListener keyup keyUpHandler false windowTarget

    void $ flip requestAnimationFrame w do
      step w aVar ps

    pure unit
