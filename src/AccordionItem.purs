module Halogen.Headless.AccordionItem where

import Prelude

import DOM.HTML.Indexed (HTMLh2, HTMLbutton, HTMLdiv)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Record as Record
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

defaultOpenOptions :: forall i. Record (OpenOptions i ())
defaultOpenOptions =
  { open: false
  , onOpenChange: Nothing
  }

type Options headingProps triggerProps panelProps p i =
  RenderOptions headingProps triggerProps panelProps p i + OpenOptions i + ()

defaultOptions
  :: forall p i
   . Record (Options HTMLh2 HTMLbutton HTMLdiv p i)
defaultOptions = Record.merge defaultRenderOptions defaultOpenOptions

type TriggerId = String

type PanelId = String

type Content p i = Array (HH.HTML p i)

type TriggerContent p i = Content p i

type PanelContent p i = Content p i

accordionItem
  :: forall headingProps triggerProps panelProps p i
   . Record (Options headingProps (TriggerProps triggerProps) (PanelProps panelProps) p i)
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
            ( [ HP.id triggerId
              , HPA.controls panelId
              , HPA.expanded $ if open then "true" else "false"
              ] <>
                ( case onOpenChange of
                    Just f -> [ HE.onClick \_ -> f $ not open ]
                    Nothing -> []
                )
            )
            triggerContent
        ]
    , renderPanel
        [ HP.id panelId
        , HPA.role "region"
        , HPA.labelledBy triggerId
        ]
        panelContent
    ]
