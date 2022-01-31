module Stories.Accordion where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Headless.Accordion (useAccordion)
import Halogen.Headless.Accordion as Accordion
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks

items :: forall p i. Array (Int /\ (Array (HH.HTML p i) /\ Array (HH.HTML p i)))
items =
  [ 1
      /\ "What is a headless component?"
      /\
        """
    A headless component addresses concerns like state, accessibility, and
    keyboard support while allowing you to focus on implementing your own
    visual design.
    """
  , 2
      /\ "Is the Accordion component accessible?"
      /\
        """
    The Accordion component implements the WAI-ARIA Accordion Design Pattern
    which provides an accessible markup structure to support screen reader
    and keyboard users.
    """
  , 3
      /\ "How can I customize the Accordion component?"
      /\
        """
    The Accordion component provides minimalistic default HTML. You can
    provide your own functions to render the desired heading, trigger, and
    panel markup.
    """
  ]
    <#>
      \(Tuple v (Tuple q a)) -> v /\ [ HH.text q ] /\ [ HH.text a ]

singleUncontrolled :: forall q i o m. MonadEffect m => Component q i o m
singleUncontrolled =
  Hooks.component \_ _ ->
    useAccordion
      (Accordion.defaultOptions Accordion.Single)
        { renderPanel = \open p -> HH.div (p <> if not open then [ HP.style "display: none;" ] else [])
        }
      items

singleControlled :: forall q i o m. MonadEffect m => Component q i o m
singleControlled =
  Hooks.component \_ _ -> Hooks.do
    value /\ valueId <- useState Nothing
    accordion <- useAccordion
      (Accordion.defaultOptions Accordion.Single)
        { renderPanel = \open p -> HH.div (p <> if not open then [ HP.style "display: none;" ] else [])
        , value = Just value
        , onValueChange = Just $ Hooks.put valueId
        }
      items
    Hooks.pure $
      HH.div_
      [ accordion
      , HH.p_
        [ HH.strong_ [ HH.text "Selected value: " ]
        , HH.text $ show value
        ]
      ]

multipleUncontrolled :: forall q i o m. MonadEffect m => Component q i o m
multipleUncontrolled =
  Hooks.component \_ _ ->
    useAccordion
      (Accordion.defaultOptions Accordion.Multiple)
        { renderPanel = \open p -> HH.div (p <> if not open then [ HP.style "display: none;" ] else [])
        }
      items

multipleControlled :: forall q i o m. MonadEffect m => Component q i o m
multipleControlled =
  Hooks.component \_ _ -> Hooks.do
    value /\ valueId <- useState []
    accordion <- useAccordion
      (Accordion.defaultOptions Accordion.Multiple)
        { renderPanel = \open p -> HH.div (p <> if not open then [ HP.style "display: none;" ] else [])
        , value = Just value
        , onValueChange = Just $ Hooks.put valueId
        }
      items
    Hooks.pure $
      HH.div_
      [ accordion
      , HH.p_
        [ HH.strong_ [ HH.text "Selected value: " ]
        , HH.text $ show value
        ]
      ]
