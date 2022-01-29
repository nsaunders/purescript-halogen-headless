module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object (fromFoldable) as Object
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Storybook (Stories, runStorybook, proxy)
import Stories.Accordion (component) as Accordion
import Stories.AccordionItem (component) as AccordionItem

stories :: forall m. MonadEffect m => Stories m
stories = Object.fromFoldable
  [ Tuple "" $ proxy $ Hooks.component \_ _ -> Hooks.pure $ HH.text "todo"
  , Tuple "Accordion" $ proxy Accordion.component
  , Tuple "AccordionItem" $ proxy AccordionItem.component
  ]

main :: Effect Unit
main = runHalogenAff $
  awaitBody >>=
    runStorybook
      { stories
      , logo: pure (HH.text "Halogen Headless")
      }
