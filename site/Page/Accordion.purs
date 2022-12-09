module Site.Page.Accordion where

import Prelude

import DOM.HTML.Indexed (HTMLbutton, HTMLdiv, HTMLh3)
import Data.Array ((:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Foreign.Object (insert)
import Foreign.Object as Object
import Halogen (ClassName(..), Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Headless.Accordion (useAccordion)
import Halogen.Headless.Accordion as Accordion
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Storybook (Stories)
import Site.Theme as Theme
import Tecton
  ( CSS
  , active
  , backgroundColor
  , borderStyle
  , color
  , display
  , em
  , fontFamily
  , fontSize
  , height
  , hidden
  , hover
  , inlineBlock
  , margin
  , ms
  , nil
  , none
  , overflow
  , padding
  , pct
  , px
  , rem
  , start
  , textAlign
  , transitionDuration
  , transitionProperty
  , universal
  , width
  , (&:)
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Halogen ((&.))
import Tecton.Halogen as TH
import Tecton.Rule as Rule

demos :: forall m. MonadEffect m => Stories m
demos = Object.empty
  # insert "Accordion|Single" singleUncontrolled
  # insert "Accordion|Single (Controlled)" singleControlled
  # insert "Accordion|Multiple" multipleUncontrolled
  # insert "Accordion|Multiple (Controlled)" multipleControlled

items :: forall p i. Array (Int /\ HH.HTML p i /\ HH.HTML p i)
items =
  [ 1
      /\ "What is a headless component?"
      /\
        """
    A headless component addresses concerns like state, accessibility, and
    keyboard support while allowing you to focus on implementing your own
    visual design.
    """
  , 2
      /\ "Is the Accordion component accessible?"
      /\
        """
    The Accordion component implements the WAI-ARIA Accordion Design Pattern
    which provides an accessible markup structure to support screen reader
    and keyboard users.
    """
  , 3
      /\ "How can I customize the Accordion component?"
      /\
        """
    The Accordion component provides minimalistic default HTML. You can
    provide your own functions to render the desired heading, trigger, and
    panel markup.
    """
  ]
    <#>
      \(Tuple v (Tuple q a)) -> v /\ HH.text q /\ HH.div
        [ HP.class_ contentClass ]
        [ HH.text a ]

headingClass = ClassName "accordion__heading" :: ClassName
triggerClass = ClassName "accordion__trigger" :: ClassName
triggerIndicatorClass = ClassName "accordion__trigger-indicator" :: ClassName
panelClass = ClassName "accordion__panel" :: ClassName
panelClosedClass = ClassName "accordion__panel-closed" :: ClassName
panelOpenClass = ClassName "accordion__panel-open" :: ClassName
contentClass = ClassName "accordion__content" :: ClassName

css :: CSS
css = do
  universal &. headingClass
    ? margin
    := nil
  universal &. triggerClass ? Rule.do
    margin := nil
    padding := rem 0.5 ~ rem 1
    width := pct 100
    borderStyle := none
    textAlign := start
    fontFamily := Theme.sans
    fontSize := rem 1.0
    backgroundColor := Theme.blue600
    color := Theme.blue100
  universal &. triggerClass &: hover
    ? backgroundColor
    := Theme.blue700
  universal &. triggerClass &: active
    ? backgroundColor
    := Theme.blue800
  universal &. triggerIndicatorClass ? Rule.do
    fontFamily := Theme.mono
    display := inlineBlock
    width := em 1.0
  universal &. panelClass ? Rule.do
    overflow := hidden
    transitionProperty := height
    transitionDuration := ms 250
  universal &. contentClass ? do
    padding := rem 1.0

renderHeading
  :: forall p i
   . Array (HP.IProp HTMLh3 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
renderHeading props =
  HH.h3 $ HP.class_ headingClass : props

renderTrigger
  :: forall p i
   . Boolean
  -> Array (HP.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
renderTrigger open props content =
  HH.button
    (HP.class_ triggerClass : props)
    ( HH.div [ HP.class_ triggerIndicatorClass ]
        [ HH.text (if open then "-" else "+") ] : content
    )

renderPanel
  :: forall p i
   . { open :: Boolean, targetHeight :: Maybe Number }
  -> Array (HP.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
renderPanel { targetHeight } props content =
  HH.div
    ( props
        <> [ HP.class_ panelClass ]
        <> (maybe [] (\h -> [ TH.style $ height := px h ]) targetHeight)
    )
    content

singleUncontrolled :: forall q i o m. MonadEffect m => Component q i o m
singleUncontrolled =
  Hooks.component \_ _ ->
    useAccordion
      (Accordion.defaultOptions Accordion.Single)
        { renderHeading = renderHeading
        , renderTrigger = renderTrigger
        , renderPanel = renderPanel
        }
      items

singleControlled :: forall q i o m. MonadEffect m => Component q i o m
singleControlled =
  Hooks.component \_ _ -> Hooks.do
    value /\ valueId <- useState Nothing
    accordion <- useAccordion
      (Accordion.defaultOptions Accordion.Single)
        { value = Just value
        , onValueChange = Just $ Hooks.put valueId
        , renderHeading = renderHeading
        , renderTrigger = renderTrigger
        , renderPanel = renderPanel
        }
      items
    Hooks.pure $
      HH.div_
        [ accordion
        , HH.p_
            [ HH.strong_ [ HH.text "Selected value: " ]
            , HH.text $ show value
            ]
        ]

multipleUncontrolled :: forall q i o m. MonadEffect m => Component q i o m
multipleUncontrolled =
  Hooks.component \_ _ ->
    useAccordion
      (Accordion.defaultOptions Accordion.Multiple)
        { renderHeading = renderHeading
        , renderTrigger = renderTrigger
        , renderPanel = renderPanel
        }
      items

multipleControlled :: forall q i o m. MonadEffect m => Component q i o m
multipleControlled =
  Hooks.component \_ _ -> Hooks.do
    value /\ valueId <- useState []
    accordion <- useAccordion
      (Accordion.defaultOptions Accordion.Multiple)
        { value = Just value
        , onValueChange = Just $ Hooks.put valueId
        , renderHeading = renderHeading
        , renderTrigger = renderTrigger
        , renderPanel = renderPanel
        }
      items
    Hooks.pure $
      HH.div_
        [ accordion
        , HH.p_
            [ HH.strong_ [ HH.text "Selected value: " ]
            , HH.text $ show value
            ]
        ]
