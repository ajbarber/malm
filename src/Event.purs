module Event where

import Prelude

import Data.Array (cons, delete, difference, dropEnd, filter, head, intersect, length, nub, nubEq)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EVar
import Types (AsyncState, Direction(..), EventType(..), State)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (code, key)
import Web.UIEvent.KeyboardEvent as KBD
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)

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

hook :: AVar AsyncState -> Effect Unit
hook aVar = do
  keyDownHandler <- handleEvent KeyDown aVar
  keyUpHandler <- handleEvent KeyUp aVar
  windowTarget <- map Window.toEventTarget window

  addEventListener keydown keyDownHandler false windowTarget
  addEventListener keyup keyUpHandler false windowTarget

marshall :: AVar AsyncState -> State -> Effect State
marshall aVar state = do
  event <- EVar.tryRead aVar
  dir <- case (head $ fromMaybe [] event) of
    Just ev -> keys ev state.hero.direction
    Nothing -> pure state.hero.direction
  pure state { hero { direction = dir } }
