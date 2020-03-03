module TestData where

import DataModel (User(..), SendOut(..), Message(..), Recipient(..), Status(..))

message_dlg123 :: Message
message_dlg123 = Message { id: "dlg123", text: "hi!" }

message_dlg124 :: Message
message_dlg124 = Message { id: "dlg124", text: "bye!" }

message_dlg125 :: Message
message_dlg125 = Message { id: "dlg125", text: "oops!" }

user1 :: User
user1 = User { id: "user1" }

user2 :: User
user2 = User { id: "user2" }

sendout_dlg123 :: SendOut
sendout_dlg123 = SendOut
  { message: message_dlg123
  , recipients:
    [ Recipient { user: user1, status: None }
    , Recipient { user: user2, status: None }
    ]
  }

sendout_dlg124 :: SendOut
sendout_dlg124 = SendOut
  { message: message_dlg124
  , recipients:
    [ Recipient { user: user1, status: None }
    ]
  }
