module Halogen.Headless.Accordion where

import Prelude

import Data.Array (cons, elem, filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.Headless.AccordionItem (PanelProps, TriggerProps, accordionItem)
import Halogen.Headless.AccordionItem as AccordionItem
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, HookType, Pure, UseState, useState)
import Halogen.Hooks as Hooks
import Type.Row (type (+))

type Options headingProps triggerProps panelProps a p i =
  Record
    ( AccordionItem.RenderOptions headingProps triggerProps panelProps p i
    + ( items :: Array (Tuple a (Tuple (Array (HH.HTML p i)) (Array (HH.HTML p i))))
      , value :: Maybe (Array a)
      , onValueChange :: Maybe (Array a -> i)
      )
    )

foreign import data UseAccordion :: Type -> HookType

instance HookNewtype (UseAccordion a) (UseState (Array a) <> Pure)

useAccordion
  :: forall headingProps triggerProps panelProps a p m
   . Eq a
  => Options headingProps (TriggerProps triggerProps) (PanelProps panelProps) a p (HookM m Unit)
  -> Hook m (UseAccordion a) (HH.HTML p (HookM m Unit))
useAccordion { renderHeading, renderTrigger, renderPanel, items, value: valueProp, onValueChange } =
  Hooks.wrap $
    Hooks.do
      selection /\ selectionId <- useState $ fromMaybe [] valueProp
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
      Hooks.pure $
        HH.div_ $
          items <#>
            \(Tuple v (Tuple triggerContent panelContent)) ->
              accordionItem
                AccordionItem.defaultOptions
                  { renderHeading = renderHeading
                  , renderTrigger = renderTrigger
                  , renderPanel = renderPanel
                  , open = v `elem` value
                  , onOpenChange = Just \open -> if open then select v else deselect v
                  }
                "foo"
                "bar"
                triggerContent
                panelContent
