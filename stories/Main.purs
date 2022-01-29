module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object (fromFoldable) as Object
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)
import Stories.Accordion (component) as Accordion
import Stories.AccordionItem (component) as AccordionItem
import Stories.Index (component) as Index

stories :: forall m. MonadEffect m => Stories m
stories = Object.fromFoldable
  [ "" /\ proxy Index.component
  , "Accordion" /\ proxy Accordion.component
  , "AccordionItem" /\ proxy AccordionItem.component
  ]

main :: Effect Unit
main = runHalogenAff $
  awaitBody >>=
    runStorybook
      { stories
      , logo: pure (HH.text "Halogen Headless")
      }
