{ name = "halogen-headless"
, dependencies =
  [ "arrays"
  , "console"
  , "dom-indexed"
  , "effect"
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
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "stories/**/*.purs", "test/**/*.purs" ]
}
