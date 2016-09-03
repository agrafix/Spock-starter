{-# LANGUAGE OverloadedStrings #-}
module MT.Forms where

import qualified Data.Text as T
import qualified Text.Blaze.Html as H
import Text.Digestive
import Text.Digestive.Bootstrap

data TweetReq
    = TweetReq
    { tr_content :: T.Text
    } deriving (Show)

minMaxLen :: (Int, Int) -> T.Text -> Result H.Html T.Text
minMaxLen (minLen, maxLen) t =
    if len >= minLen && len <= maxLen
    then Success stripped
    else Error $ H.toHtml $ "Must be longer than " ++ show minLen ++ " and shorter than " ++ show maxLen ++ " characters"
    where
      stripped = T.strip t
      len = T.length stripped

tweetForm :: Monad m => Maybe T.Text -> Form H.Html m TweetReq
tweetForm mTxt =
    TweetReq
    <$> "tweet" .: validate (minMaxLen (1, 142)) (text mTxt)

tweetFormSpec :: FormMeta
tweetFormSpec =
    FormMeta
    { fm_method = POST
    , fm_target = "/"
    , fm_elements =
        [ FormElement "tweet" (Just "What's on your mind?") (InputTextArea (Just 10) (Just 5))
        ]
    , fm_submitText = "Submit tweet"
    }

data LoginReq
    = LoginReq
    { lr_username :: T.Text
    , lr_password :: T.Text
    } deriving (Show, Eq)

loginForm :: Monad m => Form H.Html m LoginReq
loginForm =
    LoginReq
    <$> "user" .: text Nothing
    <*> "pass" .: text Nothing

loginFormSpec :: FormMeta
loginFormSpec =
    FormMeta
    { fm_method = POST
    , fm_target = "/login"
    , fm_elements =
         [ FormElement "user" (Just "Username") InputText
         , FormElement "pass" (Just "Password") InputPassword
         ]
    , fm_submitText = "Login"
    }
