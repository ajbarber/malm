module Event where

import Prelude

import Data.Array (cons, difference, dropEnd, filter, fromFoldable, head, intersect, length, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Debug (trace, spy)
import Effect (Effect)
import P5.Events (keyIsDown, keyReleased)
import P5.Types (P5)
import Types (Direction(..))

initialCursor :: Array Direction
initialCursor = []

direction :: Array Direction -> Direction
direction k = fromMaybe None (head k)

keys :: P5 -> Array Direction -> Effect (Array Direction)
keys p c = pure $ intersect cur n
  where
    cur = case head newVal of
      Nothing -> c
      Just x -> cons x (dropEnd 1 c)
    newVal = difference n c
    n = filter (\x -> x /= None) [ l, r, u, d]
    l = if keyIsDown p 37.0 then Left else None
    r = if keyIsDown p 39.0 then Right else None
    u = if keyIsDown p 38.0 then Up else None
    d = if keyIsDown p 40.0 then Down else None
