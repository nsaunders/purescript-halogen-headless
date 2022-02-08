module Halogen.Headless.Accordion (class SelectionMode, Single(..), Multiple(..), UseAccordion, defaultOptions, defaultValueOptions, selectionLimit, selectionFromArray, selectionToArray, useAccordion) where

import Prelude

import DOM.HTML.Indexed (HTMLh2, HTMLbutton, HTMLdiv)
import Data.Array (catMaybes, cons, elem, elemIndex, foldr, head, length, mapWithIndex, singleton, take, updateAt, (!!), (..))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Headless.Internal.ElementId (UseElementId, useElementId)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, HookType, Pure, UseEffect, UseState, useState, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Record as Record
import Type.Row (type (+))
import Web.DOM.Element as Element
import Web.DOM.NonDocumentTypeChildNode (nextElementSibling, previousElementSibling)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (currentTarget)
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLElement as HTMLElement
import Web.ResizeObserver (resizeObserver)
import Web.ResizeObserver as ResizeObserver
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
  , renderPanel :: { open :: Boolean, targetHeight :: Maybe Number } -> Render panelProps p i
  | r
  )

defaultRenderOptions
  :: forall p i
   . Record (RenderOptions HTMLh2 HTMLbutton HTMLdiv p i ())
defaultRenderOptions =
  { renderHeading: HH.h2
  , renderTrigger: const HH.button
  , renderPanel: \{ open } p -> HH.div (p <> if open then [] else [HP.style "display:none"])
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

instance HookNewtype (UseAccordion a) (UseState (Array a) <> UseElementId <> UseState (Array (Maybe Number)) <> UseEffect <> Pure)

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

      blockId <- useElementId

      let
        elementId s i = String.joinWith "_" [blockId, s, show i]
        triggerId = elementId "trigger"
        panelId = elementId "panel"
        measureId = elementId "measure"

        measureRefLabel :: Int -> H.RefLabel
        measureRefLabel = H.RefLabel <<< ("measure" <> _) <<< show

      heights /\ heightsId <- useState []

      Hooks.capturesWith
        (\a b -> length a.items == length b.items)
        { items }
        useTickEffect do
          measures <- traverse (Hooks.getHTMLElementRef <<< measureRefLabel) $ 0 .. (length items - 1)
          measuresIds <- liftEffect $ traverse (traverse (Element.id <<< HTMLElement.toElement)) measures
          heights' <- traverse (traverse $ liftEffect <<< HTMLElement.offsetHeight) measures
          Hooks.put heightsId heights'

          { emitter, listener } <- liftEffect HS.create
          subscriptionId <- Hooks.subscribe emitter
          obs <- liftEffect $
                   resizeObserver \entries _ ->
                     HS.notify listener do
                       updates <- catMaybes <$> for entries
                                    \{target,contentRect:{height}} -> do
                                      targetId <- liftEffect $ Element.id target
                                      pure
                                        $ (_ /\ height)
                                          <$> elemIndex (pure targetId) measuresIds
                       Hooks.modify_
                         heightsId $
                         flip
                           (foldr (\(ix /\ height) heights'' -> fromMaybe heights'' $ updateAt ix (pure height) heights''))
                           updates
          liftEffect $ traverse_ (traverse_ \el -> ResizeObserver.observe (HTMLElement.toElement el) {} obs) measures
          pure $ Just do
            liftEffect $ ResizeObserver.disconnect obs
            Hooks.unsubscribe subscriptionId

      let

        value = fromMaybe selection (selectionToArray mode <$> valueProp)

        open = flip elem value

        handler f s = do
          sel <- Hooks.modify selectionId $ f s
          case onValueChange of
            Just onValueChange' ->
              onValueChange' $ selectionFromArray mode sel
            Nothing ->
              pure unit

        select = handler \x -> (fromMaybe identity $ take <$> selectionLimit mode) <<< cons x

        deselect = handler \s -> Array.filter (_ /= s)

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
                  HH.div
                    [ HP.class_ $ ClassName itemClassName ]
                    [ renderHeading
                        []
                        [ renderTrigger
                            (open v)
                            [ HP.id $ triggerId i
                            , HPA.controls $ panelId i
                            , HPA.expanded $ if open v then "true" else "false"
                            , HE.onClick \_ -> v # if open v then deselect else select
                            , HE.onKeyDown $ liftEffect <<< nav
                            ]
                            [ triggerContent ]
                        ]
                    , renderPanel
                      { open: open v, targetHeight: if not (open v) then Just 0.0 else join $ heights !! i }
                      [ HP.id $ panelId i
                      , HPA.role "region"
                      , HPA.labelledBy $ triggerId i
                      ]
                      [ HH.div [HP.id $ measureId i, HP.ref $ measureRefLabel i] [panelContent] ]
                    ]
