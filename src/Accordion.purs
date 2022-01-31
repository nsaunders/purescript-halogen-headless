module Halogen.Headless.Accordion where

import Prelude

import DOM.HTML.Indexed (HTMLh2, HTMLbutton, HTMLdiv)
import Data.Array (cons, elem, filter, head, length, mapWithIndex, singleton, take, (!!))
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
import Record as Record
import Type.Row (type (+))

type ValueOptions :: forall k. (k -> Type) -> k -> Type -> Row Type -> Row Type
type ValueOptions f a i r =
  ( value :: Maybe (f a)
  , onValueChange :: Maybe (f a -> i)
  | r
  )

data Single = Single

data Multiple = Multiple

class SelectionMode mode f | mode -> f where
  selectionLimit :: mode -> Maybe Int
  selectionFromArray :: forall a. mode -> Array a -> f a
  selectionToArray :: forall a. mode -> f a -> Array a
  defaultValueOptions :: forall a i. mode -> Record (ValueOptions f a i ())

instance SelectionMode Single Maybe where
  selectionLimit _ = Just 1
  selectionFromArray _ = head
  selectionToArray _ = fromMaybe [] <<< map singleton
  defaultValueOptions _ =
    { value: Nothing
    , onValueChange: Nothing
    }

instance SelectionMode Multiple Array where
  selectionLimit _ = Nothing
  selectionFromArray _ = identity
  selectionToArray _ = identity
  defaultValueOptions _ =
    { value: Nothing
    , onValueChange: Nothing
    }

type Options :: forall k. Row Type -> Row Type -> Row Type -> k -> Type -> Type -> Type -> (k -> Type) -> Row Type
type Options headingProps triggerProps panelProps a p i mode f =
    AccordionItem.RenderOptions headingProps triggerProps panelProps p i
      + ValueOptions f a i
      + (mode :: mode)

defaultOptions
  :: forall mode f a p i
   . SelectionMode mode f
  => mode
  -> Record (Options HTMLh2 HTMLbutton HTMLdiv a p i mode f)
defaultOptions mode =
  Record.merge AccordionItem.defaultRenderOptions $ Record.merge { mode } $ defaultValueOptions mode

type Item a p i =
  Tuple a (Tuple (AccordionItem.TriggerContent p i) (AccordionItem.PanelContent p i))

foreign import data UseAccordion :: Type -> HookType

instance HookNewtype (UseAccordion a) (UseState (Array a) <> UseElementIds <> Pure)

useAccordion
  :: forall headingProps triggerProps panelProps a p m mode f
   . Eq a
  => MonadEffect m
  => SelectionMode mode f
  => Record (Options headingProps (TriggerProps triggerProps) (PanelProps panelProps) a p (HookM m Unit) mode f)
  -> Array (Item a p (HookM m Unit))
  -> Hook m (UseAccordion a) (HH.HTML p (HookM m Unit))
useAccordion { renderHeading, renderTrigger, renderPanel, mode, value: valueProp, onValueChange } items =
  Hooks.wrap $
    Hooks.do
      selection /\ selectionId <- useState $ fromMaybe [] (selectionToArray mode <$> valueProp)
      elementIds <- useElementIds $ length items * 2
      let
        value = fromMaybe selection (selectionToArray mode <$> valueProp)
        handler f s = do
          sel <- Hooks.modify selectionId $ f s
          case onValueChange of
            Just onValueChange' ->
              onValueChange' $ selectionFromArray mode sel
            Nothing ->
              pure unit
        select = handler \x -> (fromMaybe identity $ take <$> selectionLimit mode) <<< cons x
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
