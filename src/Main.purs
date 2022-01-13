module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (negateDuration)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (tryPut, tryTake) as EVar
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (new) as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Event (initialCursor, keys)
import Graphics.Canvas (CanvasImageSource, getCanvasElementById, getContext2D)
import Hero as Hero
import Image (loadImg)
import TileMap (loadedTileMap)
import Types (AsyncState, GameState(..), LoadedTile, PreloadState)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import World as World

initAsyncState :: AsyncState
initAsyncState = { event: initialCursor, location: Hero.initLoc }

main :: Effect Unit
main = launchAff_ mainA

mainA :: Aff Unit
mainA = do
  aVar <- AVar.new initAsyncState
  img <- loadImg Hero.file
  tileMap <- loadedTileMap
  liftEffect $ mainEffect aVar img tileMap

mainEffect ::
  AVar AsyncState ->
  CanvasImageSource ->
  Array LoadedTile ->
  Effect Unit
mainEffect aVar hero tiles = do
  id <- getCanvasElementById "main"
  case id of
    Just id' -> do
      ctx <- getContext2D id'
      t <- liftEffect now
      runReaderT (mainS aVar) {
        deltaTime: unInstant t,
        frameCount: 0,
        ctx: ctx,
        hero: hero,
        tileMap: tiles }
    Nothing -> log "Canvas element not found"

preload :: forall m. MonadAsk PreloadState m => m PreloadState
preload = ask

handleEvent :: Boolean -> AVar AsyncState ->  Effect EventListener
handleEvent keydown aVar = eventListener $ \e -> do
  oldM <- EVar.tryTake aVar
  let old = fromMaybe initAsyncState oldM
  newDir <- keys keydown e old.event
  _ <- EVar.tryPut old { event = newDir } aVar
  pure unit

step :: Window -> AVar AsyncState -> PreloadState -> Effect Unit
step w aVar ps = do
  t <- liftEffect now
  asM <- EVar.tryTake aVar
  let as = fromMaybe initAsyncState asM
  GameState ps' as' <- World.update (GameState (stepTime ps t) as) >>= Hero.update
  World.draw (GameState ps' as')
  Hero.draw (GameState ps' as')
  void $ EVar.tryPut as' aVar
  void $ flip requestAnimationFrame w (void $ step w aVar ps')
  where
    stepTime state time = state {
      deltaTime = unInstant time <> negateDuration (state.deltaTime),
      frameCount = state.frameCount + 1
      }

mainS :: AVar AsyncState -> ReaderT PreloadState Effect Unit
mainS aVar = do
  ps <- preload
  liftEffect do
    w <- window

    keyDownHandler <- handleEvent true aVar
    keyUpHandler <- handleEvent false aVar
    windowTarget <- map Window.toEventTarget window

    addEventListener keydown keyDownHandler false windowTarget
    addEventListener keyup keyUpHandler false windowTarget

    void $ flip requestAnimationFrame w do
      step w aVar ps

    --removeEventListener keydown handler false windowTarget

    pure unit
