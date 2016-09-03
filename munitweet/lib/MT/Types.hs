{-# LANGUAGE OverloadedStrings #-}
module MT.Types where

import Web.Spock
import Data.IORef
import Database.Persist.Sql
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type AppM a = SpockM SqlBackend MySession MyAppState a
type HandlerM a = SpockAction SqlBackend MySession MyAppState a

blaze :: Html -> HandlerM b
blaze x =
    do setHeader "Content-Type" "text/html"
       lazyBytes (renderHtml x)
