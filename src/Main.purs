module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Dashboard as Dashboard
import AppM (runAppM)
import Effect.Ref (new)
import Data.Map (empty)

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  ref <- liftEffect $ new empty
  runUI (rootComponent { storage: ref }) unit body
  where
    rootComponent env = H.hoist (runAppM env) Dashboard.component
