module Capabilities.GenerateData where

import Prelude

import DataModel
import Halogen (HalogenM)
import Control.Monad.Trans.Class (lift)
import Data.Either

class Monad m <= GenerateData m where
  generateSendout :: m (Either String SendOut)

instance generateDataHalogenM :: GenerateData m => GenerateData (HalogenM st action slots msg m) where
  generateSendout = lift $ generateSendout
