module Stories.Accordion where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.Headless.Accordion (useAccordion)
import Halogen.Hooks as Hooks

component :: forall q i o m. MonadEffect m => Component q i o m
component =
  let
    items =
      [ Tuple 1 [HH.text "Item 1"]
      , Tuple 2 [HH.text "Item 2"]
      , Tuple 3 [HH.text "Item 3"]
      , Tuple 4 [HH.text "Item 4"]
      , Tuple 5 [HH.text "Item 5"]
      ]
  in
    Hooks.component \_ _ -> Hooks.do
      accordion <- useAccordion { items, renderHeading: HH.h4, renderTrigger: HH.button, renderPanel: HH.div, value: Nothing, onValueChange: Nothing }
      Hooks.pure $ HH.div_ [accordion]
{-
component :: forall q i o m. Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  accordion <- useAccordion
                 { mode: Accordion.Single
                 , items:
                     [ Tuple 0 \{ open, toggle } ->
                         HH.div_
                         [ HH.button
                           [ HE.onClick \_ -> toggle ]
                           [ HH.text $ if open then "open" else "closed" ]
                         ]
                     , Tuple 1 \{ open, toggle } ->
                         HH.div_
                         [ HH.button
                           [ HE.onClick \_ -> toggle ]
                           [ HH.text $ if open then "open" else "closed" ]
                         ]
                     ]
                 , initialValue: []
                 }
  Hooks.pure $ HH.div_ [accordion]
  -}
