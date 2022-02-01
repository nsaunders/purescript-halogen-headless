module Halogen.Headless.Accordion (class SelectionMode, Single(..), Multiple(..), UseAccordion, defaultOptions, defaultValueOptions, selectionLimit, selectionFromArray, selectionToArray, useAccordion) where

import Prelude

import DOM.HTML.Indexed (HTMLh2, HTMLbutton, HTMLdiv)
import Data.Array (cons, elem, filter, head, length, mapWithIndex, singleton, take, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Headless.Internal.ElementId (UseElementIds, useElementIds)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, HookType, Pure, UseState, useState)
import Halogen.Hooks as Hooks
import Record as Record
import Type.Row (type (+))
import Web.UIEvent.MouseEvent (MouseEvent)

type TriggerProps r = (id :: String, onClick :: MouseEvent | r)

type PanelProps r = (id :: String | r)

type Render a p i = Array (HP.IProp a i) -> Array (HH.HTML p i) -> HH.HTML p i

type Open = Boolean

type RenderOptions headingProps triggerProps panelProps p i r =
  ( renderHeading :: Render headingProps p i
  , renderTrigger :: Open -> Render triggerProps p i
  , renderPanel :: Open -> Render panelProps p i
  | r
  )

defaultRenderOptions
  :: forall p i
   . Record (RenderOptions HTMLh2 HTMLbutton HTMLdiv p i ())
defaultRenderOptions =
  { renderHeading: HH.h2
  , renderTrigger: const HH.button
  , renderPanel: \open p -> HH.div (p <> if open then [] else [HP.style "display:none"])
  }

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
    RenderOptions headingProps triggerProps panelProps p i
      + ValueOptions f a i
      + (mode :: mode)

defaultOptions
  :: forall mode f a p i
   . SelectionMode mode f
  => mode
  -> Record (Options HTMLh2 HTMLbutton HTMLdiv a p i mode f)
defaultOptions mode =
  Record.merge defaultRenderOptions $ Record.merge { mode } $ defaultValueOptions mode

type Item a p i =
  a /\ (TriggerContent p i) /\ (PanelContent p i)

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
        $ items
          # mapWithIndex
              \i (v /\ triggerContent /\ panelContent) ->
                accordionItem
                  { renderHeading
                  , renderTrigger
                  , renderPanel
                  , open: v `elem` value
                  , onOpenChange: \open -> if open then select v else deselect v
                  }
                  (fromMaybe "trigger" $ elementIds !! i)
                  (fromMaybe "panel" $ elementIds !! (i + length items))
                  triggerContent
                  panelContent

type TriggerId = String

type PanelId = String

type Content p i = Array (HH.HTML p i)

type TriggerContent p i = Content p i

type PanelContent p i = Content p i

accordionItem
  :: forall headingProps triggerProps panelProps p i
   . Record
     (
       RenderOptions headingProps (TriggerProps triggerProps) (PanelProps panelProps) p i
       + ( open :: Boolean
         , onOpenChange :: Open -> i
         )
     )
  -> TriggerId
  -> PanelId
  -> TriggerContent p i
  -> PanelContent p i
  -> HH.HTML p i
accordionItem { renderHeading, renderTrigger, renderPanel, open, onOpenChange } triggerId panelId triggerContent panelContent =
  HH.div_
    [ renderHeading
        []
        [ renderTrigger
            open
            ( [ HP.id triggerId
              , HPA.controls panelId
              , HPA.expanded $ if open then "true" else "false"
              , HE.onClick \_ -> onOpenChange $ not open 
              ]
            )
            triggerContent
        ]
    , renderPanel
        open
        [ HP.id panelId
        , HPA.role "region"
        , HPA.labelledBy triggerId
        ]
        panelContent
    ]
