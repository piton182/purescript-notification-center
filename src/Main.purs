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

renderNav :: H.ComponentHTML Unit () Aff
renderNav =
-- <ul class="nav">
--   <li class="nav-item">
--     <a class="nav-link active" href="#">Active</a>
--   </li>
--   <li class="nav-item">
--     <a class="nav-link" href="#">Link</a>
--   </li>
--   <li class="nav-item">
--     <a class="nav-link" href="#">Link</a>
--   </li>
--   <li class="nav-item">
--     <a class="nav-link disabled" href="#">Disabled</a>
--   </li>
-- </ul>
  HH.ul
    [ HP.class_ $ ClassName "nav nav-tabs" ]
    [ HH.li [ HP.class_ $ ClassName "nav-item" ] [ HH.a [ HP.class_ $ ClassName "nav-link active", HP.href "#" ] [ HH.text "Active" ] ]
    , HH.li [ HP.class_ $ ClassName "nav-item" ] [ HH.a [ HP.class_ $ ClassName "nav-link", HP.href "#" ] [ HH.text "Link" ] ]
    ]

renderHtml :: H.ComponentHTML Unit () Aff
renderHtml =
  HH.div_
    [ renderNav
    , render $ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [Recipient { id: "user1", status: None }]}
    ]

-- renderHtml = render $ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [Recipient { id: "user1", status: None }]}

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
      [ HP.class_ $ ClassName "blue" ]
      [ HH.table [ HP.class_ $ ClassName "table" ] $
          cons (HH.tr_ [ HH.th_ [ HH.text "User" ], HH.th_ [ HH.text "Status" ]]) $
            (\(Recipient r) -> HH.tr_ [ HH.td_ [ HH.text $ show r.id ], HH.td_ [ HH.text $ show r.status ] ]) <$> recipients
      ]

instance renderableMessage :: Renderable Message where
  render (Message { id, text }) =
    HH.div
      [ HP.class_ $ ClassName "red" ]
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
