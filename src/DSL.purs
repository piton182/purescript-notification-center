module DSL where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Free (Free(..), liftF, runFreeM)
import Control.Monad.State (State, execState, evalState, get, put, modify_)
import Data.Array (cons, length)
import Effect.Aff (Aff)
import DataModel (Inbox, Message, mkMessage)

-- main :: Effect Unit
-- main = do
--   let initial = ["bye"]
--   let x = execState (interpretDSL $ script) initial
--   log $ show x
--   pure unit

runDSL :: forall a. Inbox -> DSL a -> Aff a
runDSL initial dsl = do
  let x = evalState (interpretDSL $ dsl) initial
  pure x

-- script :: DSL Unit
-- script = do
--     let msg = mkMessage "hi"
--     _ <- checkInbox
--     sendMessage msg
--     _ <- checkInbox
--     pure unit
--     -- markAsRead msg

data DSLF next =
  CheckInbox (Inbox -> next)
  | SendMessage Message next
  | MarkAsRead Message next

derive instance functorDSLF :: Functor DSLF

type DSL = Free DSLF

checkInbox :: DSL Inbox
checkInbox = liftF $ CheckInbox identity

sendMessage :: Message -> DSL Unit
sendMessage msg = liftF $ SendMessage msg unit

markAsRead :: Message -> DSL Unit
markAsRead msg = liftF $ MarkAsRead msg unit

-- type Interp = Effect

-- interpretDSL :: DSL Unit -> Interp Unit
-- interpretDSL dsl = runFreeM interpretDSLF dsl

-- interpretDSLF :: DSLF (DSL Unit) -> Interp (DSL Unit)
-- interpretDSLF (CheckInbox g) = do
--   log $ "checkInbox"
--   pure $ g empty
-- interpretDSLF (SendMessage msg next) = do
--   log $ "sendMessage" <> " " <> show msg
--   pure next
-- interpretDSLF (MarkAsRead msg next) = do
--   log $ "markAsRead" <> " " <> show msg
--   pure next

type Interp = State Inbox

interpretDSL :: forall a. DSL a -> Interp a
interpretDSL dsl = runFreeM interpretDSLF dsl

interpretDSLF :: forall a. DSLF (DSL a) -> Interp (DSL a)
interpretDSLF (CheckInbox g) = do
  inbox <- get
  pure $ g inbox
interpretDSLF (SendMessage msg next) = do
  modify_ $ cons msg
  pure next
interpretDSLF (MarkAsRead msg next) = do
  put []
  pure next

-- runFree  :: (f (Free f a) ->    Free f a ) -> Free f a ->   a
-- runFreeM :: (f (Free f a) -> m (Free f a)) -> Free f a -> m a

-- data ContentF a =
--   TextContent String a
--   | ElementContent Element a

-- derive instance functorContentF :: Functor ContentF

-- type Content = Free ContentF

-- text :: String -> Content Unit

-- elem :: Element -> Content UnitD

-- -- interpretation

-- -- render :: Content -> value
-- render :: Element -> String
-- render = execWriter <<< renderElement
--   where
--     -- renderElement :: Content ~> M
--     renderElement :: Element -> Writer String Unit
--     renderElement (Element e) = do
--       tell "<"
--       tell e.name
--       for_ e.attribs $ \x -> do
--         tell " "
--         renderAttribute x
--       renderContent e.content

--     renderContent :: Content Unit -> Writer String Unit
--     renderContent content = do
--       tell ">"
--       runFreeM renderContentItem content -- runFreeM :: (f (Free f a) -> m (Free f a)) -> Free f a -> m a
--       tell "</"
--       ...

--     renderContentItem :: ContentF (Content Unit) -> Writer String (Content Unit)
--     renderContentItem (TextContent s rest) = do
--       tell s
--       pure rest
--     renderContentItem (ElementContent e rest) = do
--       renderElement e
--       pure rest

-- log $ render $ p [] $ do
--   elem $
