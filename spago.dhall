{ name = "halogen-headless"
, dependencies =
  [ "arrays"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "record"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-resize-observer"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
