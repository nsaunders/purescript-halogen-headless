module Stories.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object as Object
import Foreign.Object (insert)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook)
import Stories.Accordion as Accordion
import Stories.Index as Index

stories :: forall m. MonadEffect m => Stories m
stories =
  Object.empty
    # Object.union Accordion.stories
    # insert "" Index.component

main :: Effect Unit
main = HA.runHalogenAff $
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Just $ HH.text "Halogen Headless"
    }
