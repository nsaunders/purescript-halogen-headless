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

data Single = Single

data Multiple = Multiple

class SelectionMode mode f | mode -> f where
  selectionLimit :: mode -> Maybe Int
  selectionFromArray :: forall a. mode -> Array a -> f a
  selectionToArray :: forall a. mode -> f a -> Array a

instance SelectionMode Single Maybe where
  selectionLimit _ = Just 1
  selectionFromArray _ = head
  selectionToArray _ = fromMaybe [] <<< map singleton

instance SelectionMode Multiple Array where
  selectionLimit _ = Nothing
  selectionFromArray _ = identity
  selectionToArray _ = identity

type ValueOptions mode f a i r =
  ( mode :: mode
  , value :: Maybe (f a)
  , onValueChange :: Maybe (f a -> i)
  | r
  )

defaultValueOptions :: forall a i. Record (ValueOptions Single Maybe a i ())
defaultValueOptions =
  { mode: Single
  , value: Nothing
  , onValueChange: Nothing
  }

type Options headingProps triggerProps panelProps a p i mode f =
    AccordionItem.RenderOptions headingProps triggerProps panelProps p i
      + ValueOptions mode f a i
      + ()

defaultOptions :: forall a p i. Record (Options HTMLh2 HTMLbutton HTMLdiv a p i Single Maybe)
defaultOptions =
  Record.merge AccordionItem.defaultRenderOptions defaultValueOptions

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
