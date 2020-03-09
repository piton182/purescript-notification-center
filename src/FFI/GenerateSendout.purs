module FFI.GenerateSendout where

import Prelude

import DataModel
import Effect
import DataModel
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either

foreign import _generateSendout :: Effect String

generateSendout :: Effect (Either String SendOut)
generateSendout = do
  str <- _generateSendout
  pure $ decodeJson =<< jsonParser str
