{ name = "halogen-headless"
, dependencies =
  [ "arrays"
  , "control"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
