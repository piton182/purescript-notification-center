module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (launchAff_)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff (Aff)
import Data.Const (Const)
import Data.Array (cons)
import Halogen.HTML (ClassName(..))

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  runUI mkMyComponent unit body

type MyComponent = H.ComponentHTML Unit () Aff

-- myComponent :: MyComponent
-- myComponent =
--   HH.div_
--     -- The 'div' tag takes an Array of children
--     [ HH.div_
--       [ HH.span_
--         -- as does the `span` tag
--         [ HH.text "This is text in a span!" ]
--       ]
--     , HH.button_
--       [ HH.text "You can click me, but I don't do anything." ]
--     ]

renderHtml :: H.ComponentHTML Unit () Aff
renderHtml = render $ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [Recipient { id: "user1", status: None }]}

mkMyComponent :: H.Component HH.HTML (Const Unit) Unit Void Aff
mkMyComponent =
  H.mkComponent
    { initialState: const unit
    , render: \_ -> renderHtml
    , eval: H.mkEval H.defaultEval
    }

newtype Message = Message { id :: String, text :: String }

data Status = None | Sent | Received

instance showStatus :: Show Status where
  show None = "None"
  show Sent = "Sent"
  show Received = "Received"

newtype Recipient = Recipient { id :: String, status :: Status }

newtype SendOut = SendOut ({ message :: Message, recipients :: Array Recipient })

class Renderable a where
  render :: a -> H.ComponentHTML Unit () Aff

instance rendarableSendOut :: Renderable SendOut where
  render (SendOut { message, recipients }) =
    HH.div_
      [ render message
      , render recipients
      ]

instance renderableRecipients :: Renderable (Array Recipient) where
  render recipients =
    HH.div
      [ HP.class_ $ ClassName "recipients" ]
      [ HH.span_ [ HH.text "Recipients" ]
      , HH.table_ $
          cons (HH.tr_ [ HH.th_ [ HH.text "User" ], HH.th_ [ HH.text "Status" ]]) $
            (\(Recipient r) -> HH.tr_ [ HH.td_ [ HH.text $ show r.id ], HH.td_ [ HH.text $ show r.status ] ]) <$> recipients
      ]

instance renderableMessage :: Renderable Message where
  render (Message { id, text }) =
    HH.div
      [ HP.class_ $ ClassName "message" ]
      [ HH.table_
        [ HH.tr_
          [ HH.td_
            [ HH.text "ID:" ]
          , HH.td_
            [ HH.text $ show id ]
          ]
        , HH.tr_
          [ HH.td_
            [ HH.text "Text:" ]
          , HH.td_
            [ HH.text $ show text ]
          ]
        ]
      ]
