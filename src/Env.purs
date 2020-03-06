module Env where

import Data.Map (Map)
import DataModel (User, Inbox)
import Effect.Ref (Ref)

type Storage = Map User Inbox

type Env = {
  storage :: Ref Storage
}