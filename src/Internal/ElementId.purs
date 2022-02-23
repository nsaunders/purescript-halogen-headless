module Halogen.Headless.Internal.ElementId where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, Pure, UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks

foreign import data UseElementId :: HookType

foreign import elementId :: Effect String

instance newtypeUseElementId :: HookNewtype UseElementId (UseState String <> UseEffect <> Pure)

useElementId :: forall m. MonadEffect m => Hook m UseElementId String
useElementId = Hooks.wrap $ Hooks.do
  value /\ valueId <- useState mempty
  useLifecycleEffect $ (liftEffect elementId >>= Hooks.put valueId) *> pure Nothing
  Hooks.pure value
