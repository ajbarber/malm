module Event where

import Prelude

import Data.Array (cons, difference, dropEnd, filter, head, intersect)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (traceM)
import Effect (Effect)
import Types (Direction(..))
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (code, key)
import Web.UIEvent.KeyboardEvent as KBD

initialCursor :: Array Direction
initialCursor = [ ]

direction :: Array Direction -> Direction
direction k = fromMaybe None (head k)

keys :: Boolean -> Event -> Array Direction -> Effect (Array Direction)
keys isDown e c = do
  pure $ intersect cur n
  where
    cur = case head newVal of
      Nothing -> c
      Just x -> cons x (dropEnd 1 c)
    newVal = difference n c
    k = fromMaybe "" $ key <$> KBD.fromEvent e
    n = filter (\x -> x /= None) [ l, r, u, d]
    l = if k == "ArrowLeft" && isDown then Left else None
    r = if k == "ArrowRight" && isDown then Right else None
    u = if k == "ArrowUp"  && isDown then Up else None
    d = if k == "ArrowDown"  && isDown then Down else None
