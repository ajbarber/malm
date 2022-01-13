module Image where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception  (error)
import Graphics.Canvas (CanvasImageSource, Context2D, tryLoadImage)

loadImg :: String -> Aff CanvasImageSource
loadImg f = makeAff \callback -> do
  tryLoadImage f \mci ->
    case mci of
      Just ci -> callback (Right ci)
      Nothing -> callback (Left $ error "image load error")
  pure nonCanceler
