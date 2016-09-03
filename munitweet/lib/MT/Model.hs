{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module MT.Model where

import Database.Persist.TH
import Data.Time

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|
Session
     validUntil UTCTime
     userId UserId
     deriving Show
User
     name T.Text
     password T.Text
     email T.Text
     UniqueUsername name
     UniqueEmail email
     deriving Show
Tweet
     content T.Text
     date UTCTime
     responseTo TweetId Maybe
     deriving Show
TweetTags
     tweetId TweetId
     tagId TagId
     UniquePostTag tweetId tagId
     deriving Show
Tag
     tag T.Text
     UniqueTag tag
     deriving Show
|]
