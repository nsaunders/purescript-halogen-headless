module Halogen.Headless.Accordion (class SelectionMode, Single(..), Multiple(..), UseAccordion, defaultOptions, defaultValueOptions, selectionLimit, selectionFromArray, selectionToArray, useAccordion) where

import Prelude

import DOM.HTML.Indexed (HTMLh2, HTMLbutton, HTMLdiv)
import Data.Array (cons, elem, filter, head, mapWithIndex, singleton, take)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Headless.Internal.ElementId (UseElementId, useElementId)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, HookType, Pure, UseState, useState)
import Halogen.Hooks as Hooks
import Record as Record
import Type.Row (type (+))
import Web.DOM.Element as Element
import Web.DOM.NonDocumentTypeChildNode (nextElementSibling, previousElementSibling)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (currentTarget)
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)

itemClassName = "hhe_accordion-item" :: String

type TriggerProps r = (id :: String, onClick :: MouseEvent, onKeyDown :: KeyboardEvent | r)

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

type TriggerContent p i = HH.HTML p i

type PanelContent p i = HH.HTML p i

type Item a p i =
  a /\ (TriggerContent p i) /\ (PanelContent p i)

foreign import data UseAccordion :: Type -> HookType

instance HookNewtype (UseAccordion a) (UseState (Array a) <> UseElementId <> Pure)

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
      id <- useElementId
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
        nav e =
          let
            selectSibling f =
              for_ (currentTarget (KeyboardEvent.toEvent e) >>= Element.fromEventTarget) \trigger -> do
                maybeContainer <- Element.closest (QuerySelector $ "." <> itemClassName) trigger
                for_ maybeContainer \container -> do
                  element <- f container
                  maybeTrigger <- querySelector (QuerySelector "[aria-controls]") $ Element.toParentNode element
                  traverse_ focus $ maybeTrigger >>= HTMLElement.fromElement
          in
            case key e of
              "ArrowUp" ->
                selectSibling \el -> map (fromMaybe el) $ previousElementSibling $ Element.toNonDocumentTypeChildNode el
              "ArrowDown" ->
                selectSibling \el -> map (fromMaybe el) $ nextElementSibling $ Element.toNonDocumentTypeChildNode el
              "Home" ->
                let
                  go el = previousElementSibling (Element.toNonDocumentTypeChildNode el) >>= map go >>> fromMaybe (pure el)
                in
                  selectSibling go
              "End" ->
                let
                  go el = nextElementSibling (Element.toNonDocumentTypeChildNode el) >>= map go >>> fromMaybe (pure el)
                in
                  selectSibling go
              _ ->
                pure unit
      Hooks.pure
        $ HH.div_
        $ items
          # mapWithIndex
              \i (v /\ triggerContent /\ panelContent) ->
                let
                  open = v `elem` value
                  elementId = id <> "_" <> show i
                  triggerId = elementId <> "_trigger"
                  panelId = elementId <> "_panel"
                  measureId = elementId <> "_measure"
                in
                  HH.div
                    [ HP.class_ $ ClassName itemClassName ]
                    [ renderHeading
                        []
                        [ renderTrigger
                            open
                            [ HP.id triggerId
                            , HPA.controls panelId
                            , HPA.expanded $ if open then "true" else "false"
                            , HE.onClick \_ -> v # if open then deselect else select
                            , HE.onKeyDown $ liftEffect <<< nav
                            ]
                            [ triggerContent ]
                        ]
                    , renderPanel
                      open
                      [ HP.id panelId
                      , HPA.role "region"
                      , HPA.labelledBy triggerId
                      ]
                      [ HH.div [HP.id measureId] [panelContent] ]
                    ]
