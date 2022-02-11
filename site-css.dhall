let conf = ./site.dhall

in conf // {
  sources = conf.sources # ["site/CSS.purs"],
  dependencies = conf.dependencies # ["console", "either", "exceptions", "node-buffer", "node-fs", "node-process"]
}
