module Site.CSS where

import Prelude

import CSS (render, renderedSheet)
import Data.Array (elemIndex, (!!))
import Data.Either (either)
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Class.Console (log, warn)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Async (writeTextFile)
import Node.Process (argv)
import Site.Page.Accordion as Accordion

main :: Effect Unit
main = do
  args <- argv
  let to = fromMaybe "./index.css" $ "--to" `elemIndex` args >>= \i -> args !! (i + 1)
  maybe
    (warn "Warning: No CSS output")
    (\cssString ->
      writeTextFile UTF8 to cssString $
        either throwException \_ -> log "Successfully wrote CSS output."
    )
    $ renderedSheet $ render Accordion.css
