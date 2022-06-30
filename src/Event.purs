module Event where

import Prelude

import Data.Array (cons, filter, head, null)
import Data.Foldable (for_, or)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as EVar
import Effect.Ref (Ref)
import Effect.Ref (read, write) as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import Types (Action(..), AsyncState, Direction(..), DirectionTick(..), EventType(..), InputEvent(..), Movement(..), State, attackFrames)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KBD
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)

dirDecoder :: String -> DirectionTick
dirDecoder str = dir str where
  dir s = case s of
    "ArrowLeft" -> DirectionTick Left 1.0
    "ArrowRight" -> DirectionTick Right 1.0
    "ArrowUp" -> DirectionTick Up 1.0
    "ArrowDown" -> DirectionTick Down 1.0
    _ -> DirectionTick None 0.0

actionDecoder :: String -> Action
actionDecoder str = case str of
  "a" -> Attacking attackFrames
  _ -> Default

eventToString :: Event -> String
eventToString e = fromMaybe "" $ KBD.key <$> KBD.fromEvent e

handleEvent ::
  EventType ->
  Array String ->
  AVar AsyncState ->
 Effect EventListener
handleEvent evType evKeys aVar = eventListener do
    \e -> for_ (filtered e) do \n -> handleEventInner evType aVar n
  where
    filtered e = if matching e || null evKeys then Just e else Nothing
    matching e = or $ map (\x -> eventToString e == x) evKeys

handleEventInner ::
  EventType ->
  AVar AsyncState ->
  Event ->
  Effect Unit
handleEventInner evType aVar e = do
  prev <- EVar.tryTake aVar
  let prev' = fromMaybe [] prev
  let cur' = case evType of
        KeyUp -> filter (\n -> n /= Tuple (eventToString e) KeyDown) prev'
        KeyDown -> cons (Tuple (eventToString e) KeyDown) prev'
  void $ EVar.tryPut cur' aVar

hook :: AVar AsyncState ->
        Effect Unit
hook aVar = do
  keyDownAttackHandler <- handleEvent KeyDown ["a"] aVar
  keyDownControlsHandler <- handleEvent KeyDown ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"] aVar
  keyUpHandler <- handleEvent KeyUp [] aVar
  windowTarget <- map Window.toEventTarget window
  addEventListener keydown keyDownAttackHandler false windowTarget
  addEventListener keydown keyDownControlsHandler false windowTarget
  addEventListener keyup keyUpHandler false windowTarget

tick ::
  forall m.
  Eq m =>
  Monoid m =>
  (String -> m) ->
  Maybe AsyncState ->
  m
tick decoder e = fromMaybe mempty $ (decoder <<< fst) <$> (e >>= head)

marshall :: AVar AsyncState -> State -> Effect State
marshall aVar state = let h = state.hero in do
  e <- EVar.tryRead aVar
  pure $ state { hero {
                    direction = map (append $ tick dirDecoder e) h.direction,
                    action = (tick (InputEvent <<< actionDecoder) e <> h.action) } }

debounceInner ::
  forall a.
  Int ->
  Ref (Maybe TimeoutId) ->
  (a -> Effect Unit) ->
  a ->
  Effect Unit
debounceInner time ref callback a = do
  id <- Ref.read ref
  case id of
    Nothing -> schedule callback time ref a
    Just tid -> do
      clearTimeout tid
      schedule callback time ref a
  where
    schedule cb t r v = do
      tid <- setTimeout time do
        callback v
        Ref.write Nothing ref
      Ref.write (Just tid) ref
