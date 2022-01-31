module Stories.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook)
import Stories.Accordion as Accordion
import Stories.AccordionItem as AccordionItem
import Stories.Index as Index

stories :: forall m. MonadEffect m => Stories m
stories =
  Object.fromFoldable
    [ "" /\ Index.component
    , "AccordionItem" /\ AccordionItem.component
    ]
    # Object.union Accordion.stories

main :: Effect Unit
main = HA.runHalogenAff $
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Just $ HH.text "Halogen Headless"
    }
