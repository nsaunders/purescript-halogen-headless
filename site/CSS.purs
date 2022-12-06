module Site.CSS where

import Prelude

import Data.Array (elemIndex, (!!))
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Async (writeTextFile)
import Node.Process (argv)
import Site.Page.Accordion as Accordion
import Tecton (compact, renderSheet)

main :: Effect Unit
main = do
  args <- argv
  let to = fromMaybe "./index.css" $ "--to" `elemIndex` args >>= \i -> args !! (i + 1)
  let cssString = renderSheet compact Accordion.css
  writeTextFile UTF8 to cssString $
    either throwException \_ -> log "Successfully wrote CSS output."
