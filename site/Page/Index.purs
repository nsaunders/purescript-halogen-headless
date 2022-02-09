module Site.Page.Index where

import Prelude

import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ ->
  Hooks.pure $
    HH.section_
      [ HH.h1_
          [ HH.text "Halogen Headless" ]
      , HH.p_
          [ HH.text
              """
         Halogen Headless is a new, work-in-progress component library for
         """
          , HH.a
              [ HP.href "https://github.com/purescript-halogen/purescript-halogen" ]
              [ HH.text "Halogen" ]
          , HH.text
              """
         that attempts to solve concerns like state management, keyboard support,
         and accessibility while allowing you to implement your own visual
         designs.
         """
          ]
      ]
