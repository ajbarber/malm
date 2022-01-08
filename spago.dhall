{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "malm"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-generic"
  , "integers"
  , "math"
  , "maybe"
  , "numbers"
  , "p5"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
