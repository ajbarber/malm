module Main where

import Control.Alt (map)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.Traversable (for)
import Effect (Effect)
import P5 (P5, draw, getP5, setup)
import P5.Color (background2)
import P5.Image (image, loadImage)
import P5.Rendering (createCanvas)
import P5.Shape (ellipse)
import P5.Structure (pop, push)
import P5.Types (ElementOrImage(..))
import Prelude (bind, discard, negate, pure, unit, void, ($), (<$>))
import TileMap (TileMap(..), tileMap)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)

type AppState = {
  p5 :: P5
}

initialState :: Maybe AppState
initialState = Nothing

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  win <- window
  w <- toNumber <$> innerWidth win
  h <- toNumber <$> innerHeight win
  p <- maybe getP5 (\x -> pure x.p5) mAppState

  let loadedTileMap = map (\t -> { e: ElementOrImageImage (i t), loc: t.loc }) td
        where
          TileMap basePath td = tileMap
          i t = loadImage p (basePath <> "/" <> t.file) Nothing Nothing

  setup p do
   _ <- createCanvas p 800.0 800.0 Nothing
   pure unit


  draw p do
    background2 p [135.0, 206.0, 205.0]
    push p
    ellipse p (-1000.0) 200.0 100.0 $ Just 50.0

    void $ for loadedTileMap \n ->
      image p n.e n.loc.xpos n.loc.ypos (Just n.loc.w) (Just n.loc.h)

    pop p
    pure unit

  pure $ Just { p5: p }
