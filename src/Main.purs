module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Array (cons, dropEnd, head, length)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (negateDuration)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (tryPut, tryTake, tryRead) as EVar
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (new) as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Event (keys)
import Graphics.Canvas (CanvasImageSource, getCanvasElementById, getContext2D)
import Hero as Hero
import Image (loadImg)
import TileMap (loadedTileMap)
import Types (AsyncState, EventType(..), LoadedTile, State, LoadedTileMap)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
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
  img <- loadImg Hero.file
  tileMap <- loadedTileMap
  liftEffect $ mainEffect aVar img tileMap

mainEffect ::
  AVar AsyncState ->
  CanvasImageSource ->
  LoadedTileMap ->
  Effect Unit
mainEffect aVar hero tiles = do
  id <- getCanvasElementById "main"
  for_ id \id' -> do
    ctx <- getContext2D id'
    t <- liftEffect now
    runReaderT (mainS aVar) {
      deltaTime: unInstant t,
      frameCount: 0,
      ctx: ctx,
      hero: hero,
      direction: [],
      location: Hero.initLoc,
      tileMap: tiles }

preload :: forall m. MonadAsk State m => m State
preload = ask

handleEvent :: EventType -> AVar AsyncState ->  Effect EventListener
handleEvent evType aVar = eventListener $ \e -> do
  prev <- EVar.tryTake aVar
  let prev' = (fromMaybe [] prev)
  let cur = (cons (Tuple e evType) prev')
  let cur' = if length cur > 2 then dropEnd 1 cur else cur
  void $ EVar.tryPut cur' aVar

step :: Window -> AVar AsyncState -> State -> Effect Unit
step w aVar state = do
  t <- liftEffect now
  event <- EVar.tryRead aVar
  dir <- case (head $ fromMaybe [] event) of
    Just ev -> keys ev state.direction
    Nothing -> pure state.direction
  state' <- World.update (stepSt state t dir) >>= Hero.update
  World.draw state'
  Hero.draw state'
  void $ flip requestAnimationFrame w (void $ step w aVar state')
  where
    stepSt st time dir = st {
      deltaTime = unInstant time <> negateDuration (st.deltaTime),
      frameCount = st.frameCount + 1,
      direction = dir
      }

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
