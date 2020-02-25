module API where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Control.Monad.State.Trans (StateT, get, modify, put)
import Data.Identity (Identity)
import Data.Array (cons)

type Error = String
type Message = String
type Inbox = Array Message

class Monad m <= NotificationCenterAPI m where
  checkInbox :: m Inbox
  sendMessage :: Message -> m Unit
  markAsRead :: Message -> m Unit

instance notificationCenterAPIStateT :: NotificationCenterAPI (StateT (Array String) Effect) where
  checkInbox :: StateT (Array String) Effect Inbox
  checkInbox = do
    inbox <- get
    pure $ inbox
  sendMessage :: Message -> StateT (Array String) Effect Unit
  sendMessage msg = do
    _ <- modify $ cons msg
    pure $ unit
  markAsRead :: Message -> StateT (Array String) Effect Unit
  markAsRead msg = do
    _ <- put []
    pure $ unit
