module Site.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign.Object as Object
import Foreign.Object (insert)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook)
import Site.Index as Index
import Site.Demo.Accordion as Accordion

pages :: forall m. MonadEffect m => Stories m
pages =
  Object.empty
    # insert "" Index.component
    # Object.union Accordion.demos

main :: Effect Unit
main = HA.runHalogenAff $
  HA.awaitBody >>= runStorybook
    { stories: pages
    , logo: Just $ HH.text "Halogen Headless"
    }
