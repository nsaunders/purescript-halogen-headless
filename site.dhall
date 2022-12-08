let conf = ./spago.dhall

in conf // {
  sources = conf.sources # ["site/**/*.purs"],
  dependencies =
    conf.dependencies #
      [ "colors"
      , "halogen-storybook"
      , "foreign-object"
      , "tecton"
      , "tecton-halogen"
      ]
}
