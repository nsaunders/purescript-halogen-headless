module Stories.Accordion where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Headless.Accordion as Accordion
import Halogen.Headless.Accordion (useAccordion)
import Halogen.Hooks as Hooks

component :: forall q i o m. MonadEffect m => Component q i o m
component =
  let
    items =
      [ Tuple 1 $ Tuple [ HH.text "Summary 1" ] [ HH.text "Details 1" ]
      , Tuple 2 $ Tuple [ HH.text "Summary 2" ] [ HH.text "Details 2" ]
      , Tuple 3 $ Tuple [ HH.text "Summary 3" ] [ HH.text "Details 3" ]
      ]
  in
    Hooks.component \_ _ -> Hooks.do
      accordion <- useAccordion
        Accordion.defaultOptions
          { renderPanel = \open p -> HH.div (p <> if not open then [ HP.style "display: none;" ] else [])
          , limit = pure 2
          }
        items
      Hooks.pure $ HH.div_ [ accordion ]
