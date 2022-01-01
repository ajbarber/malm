module Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Debug (traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, new, tryPut, tryRead, tryTake) as EVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import P5 (Image, P5, draw, getP5, setup)
import P5.Color (background2)
import P5.Events.Keyboard (keyIsDown, keyPressed)
import P5.Image (image, image2, loadImage)
import P5.Rendering (createCanvas)
import P5.Shape (ellipse)
import P5.Types (ElementOrImage(..))
import Prelude (Unit, bind, discard, map, pure, unit, ($), (-), (<>))
import TileMap (TileMap(..), LoadedTile, tileMap)

type AppState = {
  p5 :: P5
}

eVar :: Effect (AVar Pos)
eVar = EVar.empty

initialState :: Maybe AppState
initialState = Nothing

character :: P5 -> Image
character p = loadImage p "assets/Robot/Tilesheet/character_robot_sheet.png" Nothing Nothing

loadedTileMap :: P5 -> Array LoadedTile
loadedTileMap p = map (\t -> { e: ElementOrImageImage (i t), loc: t.loc }) td
  where
    TileMap basePath td = tileMap
    i t = loadImage p (basePath <> "/" <> t.file) Nothing Nothing

type Pos = { x:: Number, y::Number }

drawStep :: P5 -> AVar Pos -> Image -> Effect Unit
drawStep p avar chr = do
  newVal <- EVar.tryRead avar
  case newVal of
    Just nv -> do
      image2 p (ElementOrImageImage $ chr) nv.x nv.y 48.0 64.0 0.0 512.0 (Just 96.0) (Just 128.0)
    Nothing -> log "error"

type State = { hero :: Image, tileMap :: Array LoadedTile }

main :: Maybe AppState -> Effect (Maybe AppState)
main a = do
    p <- maybe getP5 (\x -> pure x.p5) a
    liftEffect $ runReaderT (mainR p) { hero: character p,
                                        tileMap: loadedTileMap p }

preload :: forall m. MonadAsk State m => m State
preload = ask

mainR :: P5 -> ReaderT State Effect (Maybe AppState)
mainR p = do
  st <- preload
  liftEffect $ mainInner st p

mainInner :: State -> P5 -> Effect (Maybe AppState)
mainInner st p = do
  let initPos = { x: 100.0, y: 100.0}

  eVarInner <- EVar.new initPos

  setup p do
    _ <- createCanvas p 800.0 800.0 Nothing
    pure unit

  keyPressed p do
    let l = keyIsDown p 37.0
    mNewPos <- EVar.tryTake eVarInner
    let newPos = fromMaybe initPos mNewPos
    res <- case mNewPos of
      Just res -> do
        log $ toString res.x
        EVar.tryPut { x: res.x - 5.0, y: res.y } eVarInner
      Nothing -> pure false
    traceM res
    pure false

  draw p do
    background2 p [135.0, 206.0, 205.0]
    for_ st.tileMap \n ->
      image p n.e n.loc.xpos n.loc.ypos (Just n.loc.w) (Just n.loc.h)
    ellipse p (100.0) 200.0 100.0 $ Just 50.0

    drawStep p eVarInner st.hero
    pure unit


  --   image2 p (ElementOrImageImage $ character p) newPos.x newPos.y 48.0 64.0 0.0 512.0 (Just 96.0) (Just 128.0)

  pure $ Just { p5: p }
