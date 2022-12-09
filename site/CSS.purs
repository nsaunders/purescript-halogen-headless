module Site.CSS where

import Prelude

import Site.Page.Accordion as Accordion
import Site.Storybook as Storybook
import Tecton (pretty, renderSheet)

sheet :: String
sheet = renderSheet pretty do
  Storybook.css
  Accordion.css
