module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Dashboard as Dashboard
import DSL (runDSL)
import DataModel (mkMessage)

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  runUI rootComponent unit body
  where
    rootComponent = H.hoist (runDSL [mkMessage "hi"]) Dashboard.component
