module Stories.Accordion where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
                     { renderHeading: HH.h4
                     , renderTrigger: const HH.button
                     , renderPanel: \open p -> HH.div (p <> if not open then [HP.style "display: none;"] else [])
                     , value: Nothing
                     , onValueChange: Nothing
                     }
                     items
      Hooks.pure $ HH.div_ [ accordion ]
