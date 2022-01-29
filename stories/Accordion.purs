module Stories.Accordion where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
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
      [ 1 /\
        "What is a headless component?" /\ """
        A headless component addresses concerns like state, accessibility, and
        keyboard support while allowing you to focus on implementing your own
        visual design.
        """
      , 2 /\
        "Is the Accordion component accessible?" /\ """
        The Accordion component implements the WAI-ARIA Accordion Design Pattern
        which provides an accessible markup structure to support screen reader
        and keyboard users.
        """
      , 3 /\
        "How can I customize the Accordion component?" /\ """
        The Accordion component provides minimalistic default HTML. You can
        provide your own functions to render the desired heading, trigger, and
        panel markup.
        """
      ]
      <#>
        \(Tuple v (Tuple q a)) -> v /\ [HH.text q] /\ [HH.text a]
  in
    Hooks.component \_ _ -> Hooks.do
      accordion <- useAccordion
        Accordion.defaultOptions
          { renderPanel = \open p -> HH.div (p <> if not open then [ HP.style "display: none;" ] else [])
          , limit = pure 2
          }
        items
      Hooks.pure $ HH.div_ [ accordion ]
