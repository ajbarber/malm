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
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "math"
  , "maybe"
  , "now"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
