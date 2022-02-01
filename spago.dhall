{ name = "halogen-headless"
, dependencies =
  [ "arrays"
  , "console"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "halogen"
  , "halogen-hooks"
  , "halogen-storybook"
  , "maybe"
  , "prelude"
  , "psci-support"
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
, sources = [ "src/**/*.purs", "stories/**/*.purs", "test/**/*.purs" ]
}
