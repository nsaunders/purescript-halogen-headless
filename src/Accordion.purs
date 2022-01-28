module Halogen.Headless.Accordion where

import Prelude

import Data.Array ((!!), cons, elem, filter, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.Headless.AccordionItem (PanelProps, TriggerProps, accordionItem)
import Halogen.Headless.AccordionItem as AccordionItem
import Halogen.Headless.Internal.ElementId (UseElementIds, useElementIds)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, HookType, Pure, UseState, useState)
import Halogen.Hooks as Hooks
import Type.Row (type (+))

type ValueOptions a i r =
  ( value :: Maybe (Array a)
  , onValueChange :: Maybe (Array a -> i)
  | r
  )

type Options headingProps triggerProps panelProps a p i =
  Record
    ( AccordionItem.RenderOptions headingProps triggerProps panelProps p i
        + ValueOptions a i
        + ()
    )

type Item a p i =
  Tuple a (Tuple (AccordionItem.TriggerContent p i) (AccordionItem.PanelContent p i))

foreign import data UseAccordion :: Type -> HookType

instance HookNewtype (UseAccordion a) (UseState (Array a) <> UseElementIds <> Pure)

useAccordion
  :: forall headingProps triggerProps panelProps a p m
   . Eq a
  => MonadEffect m
  => Options headingProps (TriggerProps triggerProps) (PanelProps panelProps) a p (HookM m Unit)
  -> Array (Item a p (HookM m Unit))
  -> Hook m (UseAccordion a) (HH.HTML p (HookM m Unit))
useAccordion { renderHeading, renderTrigger, renderPanel, value: valueProp, onValueChange } items =
  Hooks.wrap $
    Hooks.do
      selection /\ selectionId <- useState $ fromMaybe [] valueProp
      elementIds <- useElementIds $ length items * 2
      let
        value = fromMaybe selection valueProp
        handler f s = do
          sel <- Hooks.modify selectionId $ f s
          case onValueChange of
            Just onValueChange' ->
              onValueChange' sel
            Nothing ->
              pure unit
        select = handler cons
        deselect = handler \s -> filter (_ /= s)
      Hooks.pure
        $ HH.div_
        $
          items
            # mapWithIndex
                \i (Tuple v (Tuple triggerContent panelContent)) ->
                  accordionItem
                    AccordionItem.defaultOptions
                      { renderHeading = renderHeading
                      , renderTrigger = renderTrigger
                      , renderPanel = renderPanel
                      , open = v `elem` value
                      , onOpenChange = Just \open -> if open then select v else deselect v
                      }
                    (fromMaybe "trigger" $ elementIds !! i)
                    (fromMaybe "panel" $ elementIds !! (i + length items))
                    triggerContent
                    panelContent
