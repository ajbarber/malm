module Event where

import Prelude

import Debug (trace, spy)
import Effect (Effect)
import P5.Events (keyIsDown)
import P5.Types (P5)
import Types (Direction(..))

initialCursor :: Direction
initialCursor = None

tick :: P5 -> Effect Direction
tick p = pure res
  where
    res = if keyIsDown p 37.0 then Left
          else if keyIsDown p 39.0 then Right
          else if keyIsDown p 38.0 then Up
          else if keyIsDown p 40.0 then Down
          else  None
