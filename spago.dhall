{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "malm"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "numbers"
  , "p5"
  , "phaser"
  , "prelude"
  , "psci-support"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
