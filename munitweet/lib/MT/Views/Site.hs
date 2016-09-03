{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module MT.Views.Site where

import Control.Monad
import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Bootstrap

styles :: [(AttributeValue, AttributeValue)]
styles =
    [ ( "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
      , "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
      )
    , ( "sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp"
      , "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
      )
    ]

scripts :: [(AttributeValue, AttributeValue)]
scripts =
    [ ( "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa"
      , "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
      )
    ]

siteView :: Html
siteView =
    html $
    do head $
           do title "Introduction page."
              forM_ styles $ \(checksum, url) ->
                  link
                      ! rel "stylesheet"
                      ! type_ "text/css"
                      ! customAttribute "crossorigin" "anonymous"
                      ! href url
                      ! customAttribute "integrity" checksum
              link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
       body $
           do mainNavigation "/" "MuniTweet" []
              container $
                  do div ! class_ "page-header"
                         $ h1 "MuniTweet"
                     p "Hello World"
              forM_ scripts $ \(checksum, url) ->
                  script
                      ! type_ "text/javascript"
                      ! customAttribute "crossorigin" "anonymous"
                      ! src url
                      ! customAttribute "integrity" checksum
                      $ mempty
