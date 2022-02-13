module Event where

import Prelude

import Data.Array (cons, dropEnd, filter, head, length)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EVar
import Types (AsyncState, Direction(..), EventType(..), State)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (key)
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

directionKeys :: Tuple Event EventType -> Array Direction -> Array Direction
directionKeys (Tuple e evType) c = case evType of
  KeyDown -> [ fromKeyString k]
  KeyUp ->  filter (\n -> n /= fromKeyString k) c
  where
    k = fromMaybe "" $ key <$> KBD.fromEvent e

handleEvent :: EventType -> AVar AsyncState -> Effect EventListener
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
marshall aVar state = let d = state.hero.direction in do
  e <- EVar.tryRead aVar
  pure $ state { hero { direction = foldl (flip directionKeys) d (e >>= head) } }
