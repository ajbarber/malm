module Event where

import Prelude

import Data.Array (cons, delete, difference, dropEnd, filter, head, intersect, length, nub, nubEq)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EVar
import Types (Direction(..), EventType(..), AsyncState)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.UIEvent.KeyboardEvent (code, key)
import Web.UIEvent.KeyboardEvent as KBD

initialCursor :: Array Direction
initialCursor = [ ]

direction :: Array Direction -> Direction
direction k = fromMaybe None (head k)

fromKeyString :: String -> Direction
fromKeyString str = case str of
  "ArrowLeft" -> Left
  "ArrowRight" -> Right
  "ArrowUp" -> Up
  "ArrowDown" -> Down
  _ -> None

keys :: Tuple Event EventType -> Array Direction -> Effect (Array Direction)
keys (Tuple e evType) c = pure case evType of
  KeyDown -> [ fromKeyString k]
  KeyUp ->  filter (\n -> n /= fromKeyString k) c
  where
    k = fromMaybe "" $ key <$> KBD.fromEvent e

handleEvent :: EventType -> AVar AsyncState ->  Effect EventListener
handleEvent evType aVar = eventListener $ \e -> do
  prev <- EVar.tryTake aVar
  let prev' = (fromMaybe [] prev)
  let cur = (cons (Tuple e evType) prev')
  let cur' = if length cur > 2 then dropEnd 1 cur else cur
  void $ EVar.tryPut cur' aVar
