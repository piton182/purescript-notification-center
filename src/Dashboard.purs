module Dashboard where

import Prelude
import TestData

import Capabilities.CheckInbox (class CheckInbox, checkInbox)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map, insert, empty, lookup)
import Data.Maybe (Maybe(..))
import DataModel (User(..), Inbox, SendOut(..), Message(..), Recipient(..), Status(..))
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.Event.EventTypes (offline)
import Data.Array (cons)
import Capabilities.GenerateData (class GenerateData, generateSendout)
import Data.Either

type State =
  { activeSendout :: SendOut
  , sendouts :: Array SendOut
  , inboxes :: Map User Inbox }

data Action = SwitchTab SendOut | CheckInbox User | NewSendout

component :: forall q i o m. GenerateData m => CheckInbox m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { activeSendout: sendout_dlg123
  , sendouts:
    [ sendout_dlg123
    , sendout_dlg124
    ]
  , inboxes: empty
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ $ ClassName "container-fluid" ]
    [ HH.div [ HP.class_ $ ClassName "row" ]
      [ HH.div [ HP.class_ $ ClassName "col" ]
        [ renderNav ]
      , HH.div [ HP.class_ $ ClassName "col-10" ]
        [ renderSendout state.activeSendout ]
      ]
    ]
  where
    renderSendout (SendOut { message, recipients }) =
      HH.div [ HP.class_ $ ClassName "container-fluid" ]
      [ HH.div [ HP.class_ $ ClassName "row" ]
        [ HH.div [HP.class_ $ ClassName "col" ]
          [ renderMessage message ]
        , HH.div [HP.class_ $ ClassName "col" ]
          [ HH.div_
            [ HH.table [ HP.class_ $ ClassName "table" ] $
              (renderRecipientAndInbox state.inboxes <$> recipients)
            ]
          ]
        ]
      ]
    renderMessage (Message { id, text }) =
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
    renderRecipientAndInbox inboxes (Recipient r) =
      HH.tr_
        [ HH.td_ [ HH.text $ show r.user ]
        , HH.td_
          [ HH.text $ showInbox inbox
          , HH.a [ HP.href "#", HE.onClick \_ -> Just $ CheckInbox r.user ] [ HH.text "(check)" ] ]
        ]
      where
        showInbox :: Maybe Inbox -> String
        showInbox Nothing = "[]"
        showInbox (Just inbox) = show inbox
        inbox :: Maybe Inbox
        inbox = lookup r.user inboxes
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
        [ HP.class_ $ ClassName "nav nav-tabs flex-column" ] $
        (renderNavItem <$> state.sendouts) <> (
          [ HH.li [ HP.class_ $ ClassName "nav-item" ]
            [ HH.a
               [ HP.class_ $ ClassName ("nav-link")
               , HP.href "#"
               , HE.onClick \_ -> Just NewSendout
               ]
               [ HH.text "(new)" ]
            ]
          ]
        )
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

handleAction :: forall o m.
  CheckInbox m
  => GenerateData m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SwitchTab sendout -> do
    H.modify_ \st -> st { activeSendout = sendout }
  CheckInbox user -> do
    inbox <- lift $ checkInbox user
    case inbox of
      Nothing -> pure unit
      Just inbox' ->
        H.modify_ \st -> st { inboxes = insert user inbox' st.inboxes }
  NewSendout -> do
    newSendout' <- generateSendout
    case newSendout' of
      Left _ -> pure unit
      Right newSendout ->
        H.modify_ \st -> st { sendouts = cons newSendout st.sendouts, activeSendout = newSendout }
