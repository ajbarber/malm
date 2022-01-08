module P5.Text where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import P5 (P5)

foreign import _text :: EffectFn4 P5 String Number Number Unit

text :: P5 -> String -> Number -> Number -> Effect Unit
text p s x y = runEffectFn4 _text  p s x y
