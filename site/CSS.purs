module Site.CSS where

import Prelude
import CSS (render, renderedSheet)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, warn)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Async (writeTextFile)
import Site.Demo.Accordion as Accordion

main :: Effect Unit
main =
  case renderedSheet $ render Accordion.css of
    Just cssString ->
      writeTextFile UTF8 "./public/app.css" cssString $
        either throwException \_ -> log "Successfully wrote CSS output."
    Nothing ->
      warn "Warning: No CSS output."
