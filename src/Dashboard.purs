module Dashboard where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))

import DataModel as DM
import DataModel (SendOut(..), Message(..), Recipient(..), Status(..))

type State = { activeSendout :: SendOut, sendouts :: Array SendOut }

data Action = SwitchTab SendOut

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { activeSendout: SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [ Recipient { id: "user1", status: None }, Recipient { id: "user2", status: None } ]}
  , sendouts:
    [ SendOut { message: Message { id: "dlg123", text: "hi!" }, recipients: [ Recipient { id: "user1", status: None }, Recipient { id: "user2", status: None } ]}
    , SendOut { message: Message { id: "dlg124", text: "bye!" }, recipients: [ Recipient { id: "user1", status: None } ] }
    ]
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ $ ClassName "container-fluid" ]
    [ HH.div [ HP.class_ $ ClassName "row" ]
      [ HH.div [ HP.class_ $ ClassName "col" ]
        [ renderNav state ]
      , HH.div [ HP.class_ $ ClassName "col-10" ]
        [ DM.render $ state.activeSendout ]
      ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SwitchTab sendout -> do
    H.modify_ \st -> st { activeSendout = sendout }

renderNav :: forall m. State -> H.ComponentHTML Action () m
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
    sendout2slug (SendOut { message: Message { id: slug } }) = slug
    renderNavItem sendout =
      HH.li [ HP.class_ $ ClassName "nav-item" ]
        [ HH.a
          [ HP.class_ $ ClassName ("nav-link" <> (if sendout `eq` state.activeSendout then " active" else ""))
          , HP.href "#"
          , HE.onClick \_ -> Just $ SwitchTab $ sendout
          ]
          [ HH.text $ show $ sendout2slug sendout ]
        ]
