module Event where

import Prelude

import Data.Array (cons, dropEnd, head, length)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EVar
import Effect.Class.Console (logShow)
import Types (Action(..), AsyncState, Direction(..), DirectionTick(..), EventType(..), InputEvent(..), State, key)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KBD
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)


dirDecoder :: String -> DirectionTick
dirDecoder str = DirectionTick (dir str) 1.0 where
  dir s = case s of
    "ArrowLeft" -> Left
    "ArrowRight" -> Right
    "ArrowUp" -> Up
    "ArrowDown" -> Down
    _ -> None

actionDecoder :: String -> Action
actionDecoder str = case str of
  "a" -> Attacking
  _ -> Default

keys ::
  forall m.
  Monoid m =>
  (String -> m) ->
  Tuple Event EventType ->
  InputEvent m
keys decoder (Tuple e evType) = InputEvent (decoder k) evType
  where
    k = fromMaybe "" $ KBD.key <$> KBD.fromEvent e

handleEvent :: EventType -> AVar AsyncState -> Effect EventListener
handleEvent evType aVar = eventListener $ \e -> do
  prev <- EVar.tryTake aVar
  traceM e
  let prev' = (fromMaybe [] prev)
  let cur = (cons (Tuple e evType) prev')
  let cur' = if length cur > 2 then dropEnd 1 cur else cur
  void $ EVar.tryPut cur' aVar

hook :: AVar AsyncState ->
        Effect Unit
hook aVar = do
  keyDownHandler <- handleEvent KeyDown aVar
  keyUpHandler <- handleEvent KeyUp aVar
  windowTarget <- map Window.toEventTarget window

  addEventListener keydown keyDownHandler false windowTarget
  addEventListener keyup keyUpHandler false windowTarget

tick ::
  forall m.
  Eq m =>
  Monoid m =>
  (String -> m) ->
  Maybe AsyncState ->
  InputEvent m
tick decoder e = fromMaybe mempty $ (keys decoder) <$> (e >>=head)

marshall :: AVar AsyncState -> State -> Effect State
marshall aVar state = let h = state.hero in do
  e <- EVar.tryRead aVar
  pure $ state { hero { direction = tick dirDecoder e <> h.direction,
                        action = tick actionDecoder e <> h.action } }
