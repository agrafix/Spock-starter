module MT.Types where

import Web.Spock
import Data.IORef
import Database.Persist.Sql

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type AppM a = SpockM SqlBackend MySession MyAppState a
type HandlerM a = SpockAction SqlBackend MySession MyAppState a
