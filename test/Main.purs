module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec.Assertions (shouldEqual)
import Data.Either (Either(..), either)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (length)
import API
import Effect.Exception (Error, error) as EE
import Control.Monad.State.Trans (StateT, runStateT, get)
import Data.Tuple (Tuple(..))
import Control.Monad.Trans.Class (lift)

-- checkInbox -> 0
-- send
-- checkInbox -> 1
-- checkInbox -> 1
-- markAsRead
-- checkInbox -> 0
foo :: StateT (Array String) Effect Unit
foo = do
  inbox <- checkInbox
  (length inbox) `shouldEqual` 0
  -- log $ show inbox
  _ <- sendMessage "asdf"
  inbox' <- checkInbox
  (length inbox') `shouldEqual` 1
  -- log $ show inbox'
  inbox'' <- checkInbox
  (length inbox'') `shouldEqual` 1
  -- log $ show inbox''
  _ <- markAsRead "asdf"
  inbox''' <- checkInbox
  (length inbox'') `shouldEqual` 1
  -- log $ show inbox''
  pure $ unit

main :: Effect Unit
main = do
  _ <- runStateT foo []
  pure $ unit
