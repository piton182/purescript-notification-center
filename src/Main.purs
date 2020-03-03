module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Dashboard as Dashboard

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  runUI Dashboard.component unit body
