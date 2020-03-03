module Capabilities.CheckInbox where

import Prelude

import DataModel (User, Inbox(..))
import Control.Monad.Trans.Class (lift)
import TestData
import Effect.Aff (Aff)

import Halogen (HalogenM)

class Monad m <= CheckInbox m where
  checkInbox :: User -> m Inbox

instance checkInboxHalogenM :: CheckInbox m => CheckInbox (HalogenM st action slots msg m) where
  checkInbox = lift <<< checkInbox
