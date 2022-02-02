module Site.Index where

import Prelude
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ HH.div_ [ HH.text "TODO" ]
