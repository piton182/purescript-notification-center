module DataModel where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML (ClassName(..))
import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Newtype
import Data.Either (note)

newtype SendOut = SendOut ({ message :: Message, recipients :: Array Recipient })

newtype User = User { id :: String }

newtype Recipient = Recipient { user :: User, status :: Status }

newtype Message = Message { id :: String, text :: String }

mkMessage :: String -> Message
mkMessage msg = Message { id: "asdf", text: msg}

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

type Inbox = Array Message

instance eqUser :: Eq User where
  eq (User { id: x }) (User { id: y }) = eq x y

instance ordUser :: Ord User where
  compare (User { id: x }) (User { id: y }) = compare x y

instance arbitraryMessage :: Arbitrary Message where
  arbitrary = Message <$> arbitrary

instance arbitrarySendout :: Arbitrary SendOut where
  arbitrary = SendOut <$> arbitrary

instance arbitraryRecipient :: Arbitrary Recipient where
  arbitrary = Recipient <$> arbitrary

instance arbitraryStatus :: Arbitrary Status where
  arbitrary = (\_ -> None) <$> (arbitrary :: Gen Int)

instance arbitraryUser :: Arbitrary User where
  arbitrary = User <$> arbitrary

derive instance newtypeUser :: Newtype User _

derive newtype instance encodeJsonUser :: EncodeJson User
derive newtype instance decodeJsonUser :: DecodeJson User

derive newtype instance decodeJsonSendOut :: DecodeJson SendOut
derive newtype instance decodeJsonMesssage :: DecodeJson Message
derive newtype instance decodeJsonRecipient :: DecodeJson Recipient

instance decodeJson :: DecodeJson Status where
  decodeJson json = do
    string <- decodeJson json
    let decodeError = "Could not decode Status from " <> string
    note decodeError $ case string of
      "NONE" -> Just None
      "SENT" -> Just Sent
      "Received" -> Just Received
      _ -> Nothing

instance showSendout :: Show SendOut where
  show _ = "SendOut"
