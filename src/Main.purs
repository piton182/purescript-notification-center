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
import Data.Array (cons, head)
import Halogen.HTML (ClassName(..))

initialState :: State
initialState =
    { activeSendout: SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [ Recipient { id: "user1", status: None }, Recipient { id: "user2", status: None } ]}
    , sendouts:
        [ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [ Recipient { id: "user1", status: None }, Recipient { id: "user2", status: None } ]}
        , SendOut { message: Message { id: "dlg124", text: "bye!" }, recipients: [ Recipient { id: "user1", status: None } ] }
        ]
    }

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  runUI (mkMyComponent initialState renderHtml) unit body

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

type State = { activeSendout :: SendOut, sendouts :: Array SendOut }

-- sendouts :: Array SendOut
-- sendouts =
--   [ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [ Recipient { id: "user1", status: None }, Recipient { id: "user2", status: None } ]} ]

-- sendouts2navslugs :: Array SendOut -> Array String
-- sendouts2navslugs sendouts = (\(SendOut { message: Message { id: messageId } } ) -> messageId) <$> sendouts

renderNav :: State -> StaticHTML
renderNav state =
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
    [ HP.class_ $ ClassName "nav nav-tabs flex-column" ]
    (renderNavItem <$> state.sendouts)
  where
    -- isActiveSendout (SendOut { message: Message { id: activeSendoutSlug }}) (SendOut { message: Maaaessage { id: slug } }) = slug `eq` activeSendoutSlug
    sendout2slug (SendOut { message: Message { id: slug } }) = slug
    renderNavItem sendout =
      HH.li [ HP.class_ $ ClassName "nav-item" ]
        [ HH.a [ HP.class_ $ ClassName ("nav-link" <> (if sendout `eq` state.activeSendout then " active" else "")), HP.href "#" ]
          [ HH.text $ show $ sendout2slug sendout ]
        ]

renderHtml :: State -> StaticHTML
renderHtml state =
  HH.div [ HP.class_ $ ClassName "container-fluid" ]
    [ HH.div [ HP.class_ $ ClassName "row" ]
      [ HH.div [ HP.class_ $ ClassName "col" ]
        [ renderNav state ]
      , HH.div [ HP.class_ $ ClassName "col-10" ]
        [ render $ state.activeSendout ]
      ]
    ]

-- renderHtml = render $ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [Recipient { id: "user1", status: None }]}

type StaticHTML = H.ComponentHTML Unit () Aff
type Renderer state = (state -> StaticHTML)

mkMyComponent :: forall state. state -> Renderer state -> H.Component HH.HTML (Const Unit) Unit Void Aff
mkMyComponent state renderer =
  H.mkComponent
    { initialState: const state
    , render: renderer
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

instance eqSendOut :: Eq SendOut where
  eq (SendOut { message: Message { id: x }}) (SendOut { message: Message { id: y }}) = eq x y

class Renderable a where
  render :: a -> H.ComponentHTML Unit () Aff

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
    -- HH.div_
    --   [ render message
    --   , render recipients
    --   ]

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
