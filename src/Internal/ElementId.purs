module Halogen.Headless.Internal.ElementId where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, Pure, UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks

foreign import data UseElementIds :: HookType

foreign import elementId :: Effect String

instance newtypeUseElementIds :: HookNewtype UseElementIds (UseState (Array String) <> UseEffect <> Pure)

useElementIds :: forall m. MonadEffect m => Int -> Hook m UseElementIds (Array String)
useElementIds n = Hooks.wrap $ Hooks.do
  value /\ valueId <- useState []
  useLifecycleEffect $ ((liftEffect (replicateA n elementId)) >>= Hooks.put valueId) *> pure Nothing
  Hooks.pure value
