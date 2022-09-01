{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "malm"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "canvas"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "js-timers"
  , "lists"
  , "math"
  , "maybe"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "record"
  , "refs"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
