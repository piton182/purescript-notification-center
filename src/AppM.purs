module AppM where

import Prelude

import Effect.Aff (Aff)
import Capabilities.CheckInbox (class CheckInbox)
import DataModel (Inbox(..))
import TestData (message_dlg125)

newtype AppM a = AppM (Aff a)

runAppM :: AppM ~> Aff
runAppM (AppM m) = m

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
-- derive newtype instance monadEffectAppM :: MonadEffect AppM
-- derive newtype instance monadAffAppM :: MonadAff AppM

instance checkInboxAppM :: CheckInbox AppM where
  checkInbox _ = pure $ Inbox $ [ message_dlg125 ]
