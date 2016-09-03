{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MT.Types where

import Web.Spock
import Data.IORef
import Database.Persist.Sql
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.Text as T

data MySession
    = EmptySession
    | LoggedInSession T.Text -- username

data MyAppState = DummyAppState (IORef Int)

type AppM ctx a = SpockCtxM ctx SqlBackend MySession MyAppState a
type HandlerM ctx a = SpockActionCtx ctx SqlBackend MySession MyAppState a

blaze :: Html -> HandlerM ctx b
blaze x =
    do setHeader "Content-Type" "text/html"
       lazyBytes (renderHtml x)

runDB :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
