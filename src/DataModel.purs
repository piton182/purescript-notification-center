module DataModel where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML (ClassName(..))
import Data.Array (cons)

newtype SendOut = SendOut ({ message :: Message, recipients :: Array Recipient })

newtype Recipient = Recipient { id :: String, status :: Status }

newtype Message = Message { id :: String, text :: String }

instance eqSendOut :: Eq SendOut where
  eq (SendOut { message: Message { id: x }}) (SendOut { message: Message { id: y }}) = eq x y

-- TODO: doesn't belong here, but ok for now
class Renderable a where
  render :: forall action slots m. a -> H.ComponentHTML action slots m

data Status = None | Sent | Received

instance showStatus :: Show Status where
  show None = "None"
  show Sent = "Sent"
  show Received = "Received"

instance rendarableSendOut :: Renderable SendOut where
  render (SendOut { message, recipients }) =
    HH.div [ HP.class_ $ ClassName "container-fluid" ]
      [ HH.div [ HP.class_ $ ClassName "row" ]
        [ HH.div [HP.class_ $ ClassName "col" ]
          [ render message ]
        , HH.div [HP.class_ $ ClassName "col" ]
          [ render recipients ]
        ]
      ]

instance renderableRecipients :: Renderable (Array Recipient) where
  render recipients =
    HH.div_
      [ HH.table [ HP.class_ $ ClassName "table" ] $
          cons (HH.tr_ [ HH.th_ [ HH.text "User" ], HH.th_ [ HH.text "Status" ]]) $
            (\(Recipient r) -> HH.tr_ [ HH.td_ [ HH.text $ show r.id ], HH.td_ [ HH.text $ show r.status ] ]) <$> recipients
      ]

instance renderableMessage :: Renderable Message where
  render (Message { id, text }) =
    HH.div_
      [ HH.table [ HP.class_ $ ClassName "table" ]
        [ HH.tr_
          [ HH.td_
            [ HH.text "ID" ]
          , HH.td_
            [ HH.text $ show id ]
          ]
        , HH.tr_
          [ HH.td_
            [ HH.text "Text" ]
          , HH.td_
            [ HH.text $ show text ]
          ]
        ]
      ]
