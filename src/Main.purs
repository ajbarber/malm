module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (new) as AVar
import Effect.Class (liftEffect)
import Event (hook, marshall) as Event
import Graphics.Canvas (CanvasImageSource, getCanvasElementById, getContext2D)
import HeroAnims as HeroAnims
import NpcAnims as NpcAnims
import Scene as Scene
import TileMap (loadedTileMap)
import Types (AsyncState, LoadedTileMap, State)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)

main :: Effect Unit
main = launchAff_ mainA

mainA :: Aff Unit
mainA = do
  aVar <- AVar.new []
  heroImg <- HeroAnims.load
  npcImg <- NpcAnims.load
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
    initScene <- Scene.init ctx hero npc tiles
    runReaderT (mainS aVar) initScene
preload :: forall m. MonadAsk State m => m State
preload = ask

step :: Window -> AVar AsyncState -> State -> Effect Unit
step w aVar state = do
  s <- Event.marshall aVar state
  s' <- Scene.update s
  Scene.draw s'
  void $ flip requestAnimationFrame w (void $ step w aVar s')

mainS :: AVar AsyncState -> ReaderT State Effect Unit
mainS aVar = do
  ps <- preload
  liftEffect do
    Event.hook aVar
    w <- window
    void $ flip requestAnimationFrame w do
      step w aVar ps
