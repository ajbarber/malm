{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "malm"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "foldable-traversable"
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
