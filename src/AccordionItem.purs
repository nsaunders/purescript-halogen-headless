module Halogen.Headless.AccordionItem where

import Prelude

import DOM.HTML.Indexed (HTMLh2, HTMLbutton, HTMLdiv)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Type.Row (type (+))
import Web.UIEvent.MouseEvent (MouseEvent)

type TriggerProps r = (id :: String, onClick :: MouseEvent | r)

type PanelProps r = (id :: String | r)

type Render a p i = Array (HP.IProp a i) -> Array (HH.HTML p i) -> HH.HTML p i

type RenderOptions headingProps triggerProps panelProps p i r =
  ( renderHeading :: Render headingProps p i
  , renderTrigger :: Render triggerProps p i
  , renderPanel :: Render panelProps p i
  | r
  )

defaultRenderOptions
  :: forall p i
   . Record (RenderOptions HTMLh2 HTMLbutton HTMLdiv p i ())
defaultRenderOptions =
  { renderHeading: HH.h2
  , renderTrigger: HH.button
  , renderPanel: HH.div
  }

type OpenOptions i r =
  ( open :: Boolean
  , onOpenChange :: Maybe (Boolean -> i)
  | r
  )

type Options headingProps triggerProps panelProps p i =
  Record (RenderOptions headingProps triggerProps panelProps p i + OpenOptions i + ())

defaultOptions
  :: forall p i
   . Options HTMLh2 HTMLbutton HTMLdiv p i
defaultOptions =
  { renderHeading: HH.h2
  , renderTrigger: HH.button
  , renderPanel: HH.div
  , open: false
  , onOpenChange: Nothing
  }

accordionItem
  :: forall headingProps triggerProps panelProps p i
   . Options headingProps (TriggerProps triggerProps) (PanelProps panelProps) p i
  -> String
  -> String
  -> Array (HH.HTML p i)
  -> HH.HTML p i
accordionItem { renderHeading, renderTrigger, renderPanel, open, onOpenChange } triggerId panelId content =
  HH.div_
  [ renderHeading
    []
    [ renderTrigger
      ([ HP.id triggerId
      , HPA.controls panelId
      , HPA.expanded $ if open then "true" else "false"
      ] <> (
        case onOpenChange of
          Just f -> [ HE.onClick \_ -> f $ not open ]
          Nothing -> []
      ))
      [ HH.text "heading" ]
    ]
  , renderPanel
    [ HP.id panelId
    , HPA.role "region"
    , HPA.labelledBy triggerId
    ]
    content
  ]
