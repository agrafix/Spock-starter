{-# LANGUAGE OverloadedStrings #-}
module MT.Server
    ( launchServer )
where

import MT.Model
import MT.Types

import Web.Spock
import Web.Spock.Config

import Database.Persist.Sqlite hiding (get)
import Control.Monad.Logger
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Text as T

launchServer :: Int -> IO ()
launchServer port =
    do pool <- runNoLoggingT $ createSqlitePool "munitweet.db" 5
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       ref <- newIORef 0
       spockCfg <-
           defaultSpockCfg EmptySession (PCPool pool) (DummyAppState ref)
       runSpock port (spock spockCfg app)

app :: AppM ()
app =
    do get root $ text "Hello World!"
       get ("hello" <//> var) helloEndpoint

helloEndpoint :: T.Text -> HandlerM ()
helloEndpoint name =
    do visitorNumber <- getVisitorNumber
       text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

getVisitorNumber :: HandlerM Int
getVisitorNumber =
    do (DummyAppState ref) <- getState
       liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
