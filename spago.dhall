{ name = "halogen-headless"
, dependencies =
  [ "arrays"
  , "console"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
  , "prelude"
  , "record"
  , "refs"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-resize-observer"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
