module Stories.AccordionItem where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Headless.AccordionItem (accordionItem)
import Halogen.Headless.AccordionItem as AccordionItem
import Halogen.Hooks as Hooks

component :: forall q i o m. MonadEffect m => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $
    accordionItem
      AccordionItem.defaultOptions
        { renderHeading = \p -> HH.h3 (p <> [ HP.style "margin: 0" ])
        }
      "trigger"
      "content"
      [ HH.text "trigger" ]
      [ HH.text "content" ]
