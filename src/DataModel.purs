module DataModel where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML (ClassName(..))
import Data.Maybe (Maybe(..))

newtype SendOut = SendOut ({ message :: Message, recipients :: Array Recipient })

newtype User = User { id :: String }

newtype Recipient = Recipient { user :: User, status :: Status }

newtype Message = Message { id :: String, text :: String }

instance showMessage :: Show Message where
  show (Message { id }) = show id

instance eqSendOut :: Eq SendOut where
  eq (SendOut { message: Message { id: x }}) (SendOut { message: Message { id: y }}) = eq x y

data Status = None | Sent | Received

instance showStatus :: Show Status where
  show None = "NONE"
  show Sent = "SENT"
  show Received = "RECEIVED"

instance showUser :: Show User where
  show (User { id }) = id

newtype Inbox = Inbox (Array Message)

instance showInbox :: Show Inbox where
  show (Inbox msgs) = show msgs

instance eqUser :: Eq User where
  eq (User { id: x }) (User { id: y }) = eq x y

instance ordUser :: Ord User where
  compare (User { id: x }) (User { id: y }) = compare x y
