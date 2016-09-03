{-# LANGUAGE OverloadedStrings #-}
module MT.Server
    ( launchServer )
where

import MT.Model
import MT.Types
import MT.Forms
import MT.Views.Site

import Web.Spock
import Web.Spock.Digestive
import Web.Spock.Config

import Text.Blaze.Html (Html)
import Text.Digestive.Bootstrap (renderForm)
import Database.Persist.Sqlite hiding (get)
import Control.Monad.Logger
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import Network.Wai.Middleware.Static
import qualified Data.Text as T
import Data.Time
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS

launchServer :: Int -> IO ()
launchServer port =
    do pool <- runNoLoggingT $ createSqlitePool "munitweet.db" 5
       runNoLoggingT $ runSqlPool (runMigration migrateCore) pool
       ref <- newIORef 0
       spockCfg <-
           defaultSpockCfg EmptySession (PCPool pool) (DummyAppState ref)
       runSpock port (spock spockCfg app)

-- websocketsOr :: ConnectionOptions -> ServerApp -> Application -> Application
wsOpts =
    WS.defaultConnectionOptions

wsServerApp pc =
    do conn <- WS.acceptRequest pc
       let loop =
               do foo <- WS.receiveData conn
                  WS.sendTextData conn ("You said: " <> foo :: T.Text)
       loop

app :: AppM () ()
app =
    do middleware (staticPolicy (addBase "static"))
       middleware (websocketsOr wsOpts wsServerApp)
       getpost root mainSite
       -- /hello/MuniHac --> Hello MuniHac
       -- get ("hello" <//> var) ::
       get ("hello" <//> var) helloEndpoint
       get ("double" <//> var) doubleNumber
       getpost "login" loginSite
       prehook liftToIntContext intContextRoutes

{-
GET                            POST
/ => mainSite                  / => mainSite
/hello/var => helloEndpoint
-}

intContextRoutes :: AppM Int ()
intContextRoutes =
    get "foo" fooHandler

fooHandler :: HandlerM Int ()
fooHandler =
    getContext >>= \magicNumber -> text ("Foo" <> T.pack (show magicNumber))

-- prehook :: MonadIO m => ActionCtxT ctx m ctx' -> SpockCtxT ctx' m () -> SpockCtxT ctx m ()

liftToIntContext :: HandlerM () Int
liftToIntContext =
    do () <- getContext
       sess <- readSession
       case sess of
         EmptySession -> text "No access, sorry bro!"
         LoggedInSession _ -> pure 42

loginSite :: HandlerM ctx ()
loginSite =
    do f <- runForm "loginForm" loginForm
       case f of
         (view, Nothing) ->
             blaze $ siteView $ renderForm loginFormSpec view
         (view, Just loginReq) ->
             do if lr_username loginReq == "user" && lr_password loginReq == "password"
                    then do modifySession $ const (LoggedInSession "user")
                            redirect "/"
                    else blaze $ loginFailedView $ renderForm loginFormSpec view

mainSite :: HandlerM ctx ()
mainSite =
    do sess <- readSession
       tweetList <-
           runDB $
           selectList [] [Desc TweetDate, LimitTo 10]
       let showTweetPage tweetForm =
               blaze (mainView tweetForm $ map entityVal tweetList)
       case sess of
         EmptySession ->
             showTweetPage "Sorry, login first to post"
         LoggedInSession _ ->
             do tf <- tweetFormHandler
                blaze (mainView tf $ map entityVal tweetList)

tweetFormHandler :: HandlerM ctx Html
tweetFormHandler =
    do f <- runForm "tweetForm" (tweetForm Nothing)
       case f of
         (view, Nothing) ->
             pure $ renderForm tweetFormSpec view
         (view, Just tweetReq) ->
             do now <- liftIO getCurrentTime
                _ <- runDB $ insert (Tweet (tr_content tweetReq) now Nothing)
                redirect "/"

doubleNumber :: Int -> HandlerM ctx ()
doubleNumber n =
    text ("The double of " <> T.pack (show n) <> " is " <> T.pack (show (n * 2)))

helloEndpoint :: T.Text -> HandlerM ctx ()
helloEndpoint name =
    do visitorNumber <- getVisitorNumber
       text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

getVisitorNumber :: HandlerM ctx Int
getVisitorNumber =
    do (DummyAppState ref) <- getState
       liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
