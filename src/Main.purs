module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State (StateT(..), evalStateT, get, lift, put, runStateT)
import Data.Array (cons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..), snd, fst)
import Debug (spy, traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, new, tryPut, tryRead, tryTake) as EVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Event (direction, initialCursor, keys)
import Hero as Hero
import P5 (Image, P5, draw, getP5, setup)
import P5.Color (background2)
import P5.Environment (frameRate, frameRate2, pixelDensity2)
import P5.Events.Keyboard (keyIsDown, keyPressed, keyReleased)
import P5.Image (image, image2, loadImage)
import P5.Rendering (createCanvas)
import P5.Shape (ellipse)
import P5.Types (ElementOrImage(..))
import Prelude (Unit, bind, discard, map, pure, unit, ($), (-), (<>))
import TileMap (tileMap)
import Types (AsyncState, Direction(..), GameState(..), LoadedTile, PreloadState, TileMap(..), dest, source)

type AppState = {
  p5 :: P5
}

initialState :: Maybe AppState
initialState = Nothing

loadedTileMap :: P5 -> Array LoadedTile
loadedTileMap p = map (\t -> { e: ElementOrImageImage (i t),
                               loc: t.loc,
                               wall: t.wall}) td
  where
    TileMap basePath td = tileMap
    i t = loadImage p (basePath <> "/" <> t.file) Nothing Nothing

initAsyncState :: AsyncState
initAsyncState = { event: initialCursor, location: Hero.initLoc }

main :: Maybe AppState -> Effect (Maybe AppState)
main a = do
    p <- maybe getP5 (\x -> pure x.p5) a
    aVar <- EVar.new initAsyncState
    liftEffect $ runReaderT (mainS aVar) { p: p,
                                           hero: Hero.img p,
                                           tileMap: loadedTileMap p }

preload :: forall m. MonadAsk PreloadState m => m PreloadState
preload = ask

handleEvent :: PreloadState -> AVar AsyncState -> Effect Boolean
handleEvent ps aVar = do
  oldM <- EVar.tryTake aVar
  let old = fromMaybe initAsyncState oldM
  newDir <- keys ps.p old.event
  _ <- EVar.tryPut old { event = newDir } aVar
  pure false

mainS :: AVar AsyncState -> ReaderT PreloadState Effect (Maybe AppState)
mainS aVar = do
  ps <- preload
  liftEffect do
    frameRate2 ps.p 60.0

    setup ps.p do
      _ <- createCanvas ps.p 320.0 180.0 Nothing
      pure unit

    keyPressed ps.p (handleEvent ps aVar)
    keyReleased ps.p (handleEvent ps aVar)

    draw ps.p do
      asM <- EVar.tryTake aVar
      let as = fromMaybe initAsyncState asM
      background2 ps.p [135.0, 206.0, 205.0]
      for_ ps.tileMap \n -> do
        let src = source n.loc
        let dst = dest n.loc
        image2 ps.p n.e dst.xpos dst.ypos dst.w dst.h src.xpos src.ypos (Just src.w) (Just src.h)
      ellipse ps.p (100.0) 200.0 100.0 $ Just 50.0
      asNew <- Hero.draw (GameState ps as)
      void $ EVar.tryPut asNew aVar
      -- drawStep st.p st.eventTick st.hero

  pure $ Just { p5: ps.p }
