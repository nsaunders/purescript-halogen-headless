module Site.Demo.Accordion where

import Prelude

import CSS (StyleM, background, black, border, byClass, color, display, em, fontFamily, fontSize, hover, inlineBlock, margin, nil, padding, pct, pseudo, px, rem, solid, star, transitionDuration, transitionProperty, width, (&), (?))
import CSS.Overflow (hidden, overflow)
import CSS.TextAlign (startTextAlign, textAlign)
import DOM.HTML.Indexed (HTMLbutton, HTMLdiv, HTMLh3)
import Data.Array ((:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Foreign.Object (insert)
import Foreign.Object as Object
import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Headless.Accordion (useAccordion)
import Halogen.Headless.Accordion as Accordion
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Storybook (Stories)
import Record.Extra (mapRecord)
import Site.Theme as Theme

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
      \(Tuple v (Tuple q a)) -> v /\ HH.text q /\ HH.div [ HP.class_ hstyles.content ] [ HH.text a ]

type Styles a =
  { heading :: a
  , trigger :: a
  , triggerIndicator :: a
  , panel :: a
  , panelClosed :: a
  , panelOpen :: a
  , content :: a
  }

styles :: Styles String
styles =
  { heading: "heading"
  , trigger: "trigger"
  , triggerIndicator: "trigger-indicator"
  , panel: "panel"
  , panelClosed: "panel-closed"
  , panelOpen: "panel-open"
  , content: "content"
  }

css :: StyleM Unit
css = do
  star & byClass styles.heading ? margin nil nil nil nil
  star & byClass styles.trigger ? do
    margin nil nil nil nil
    padding (rem 0.5) (rem 1.0) (rem 0.5) (rem 1.0)
    width $ pct 100.0
    border solid (px 0.0) black
    textAlign startTextAlign
    uncurry fontFamily Theme.sans
    fontSize $ rem 1.0
    background Theme.blue600
    color Theme.blue100
  (star & byClass styles.trigger) & hover ? background Theme.blue700
  (star & byClass styles.trigger) & pseudo "active" ? background Theme.blue800
  star & byClass styles.triggerIndicator ? do
    uncurry fontFamily Theme.mono
    display inlineBlock
    width (em 1.0)
  star & byClass styles.panel ? do
    overflow hidden
    transitionProperty "height"
    transitionDuration "250ms"
  star & byClass styles.content ? do
    padding (rem 1.0) (rem 1.0) (rem 1.0) (rem 1.0)

hstyles :: Styles H.ClassName
hstyles = mapRecord H.ClassName styles

renderHeading
  :: forall p i
   . Array (HP.IProp HTMLh3 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
renderHeading props =
  HH.h3 $ HP.class_ hstyles.heading : props

renderTrigger
  :: forall p i
   . Boolean
  -> Array (HP.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
renderTrigger open props content =
  HH.button
    (HP.class_ hstyles.trigger : props)
    (HH.div [ HP.class_ hstyles.triggerIndicator ] [ HH.text (if open then "-" else "+") ] : content)

renderPanel
  :: forall p i
   . { open :: Boolean, targetHeight :: Maybe Number }
  -> Array (HP.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
renderPanel { targetHeight } props content =
  HH.div
    ([ HP.class_ hstyles.panel, HP.style $ fromMaybe "" $ (\h -> "height: " <> show h <> "px") <$> targetHeight ] <> props)
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
