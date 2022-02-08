let conf = ./spago.dhall

in conf // {
  sources = conf.sources # ["site/Page/**/*.purs", "site/Main.purs", "site/Theme.purs"],
  dependencies = conf.dependencies # ["colors", "css", "halogen-storybook", "foreign-object", "nonempty", "record-extra"]
}
