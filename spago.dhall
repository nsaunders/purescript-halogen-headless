{ name = "halogen-headless"
, dependencies =
  [ "arrays"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
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
