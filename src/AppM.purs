module AppM where

import Data.Maybe
import DataModel
import Prelude

import Capabilities.CheckInbox (class CheckInbox)
import Capabilities.GenerateData
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Map (Map, lookup, empty)
import Effect
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import TestData (message_dlg125)
import Type.Equality (class TypeEquals, from) as TE
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Reader.Class (class MonadAsk, asks, ask)
import TestData
import FFI.GenerateSendout as G

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM e (AppM m) = runReaderT m e

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TE.TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks TE.from

instance checkInboxAppM :: CheckInbox AppM where
  checkInbox :: User -> AppM (Maybe Inbox)
  checkInbox user = do
    env <- ask
    pure $ Just []

instance generateDataAppM :: GenerateData AppM where
  generateSendout = liftEffect $ G.generateSendout
