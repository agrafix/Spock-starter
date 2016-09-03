module Main where

import MT.Server
import System.Environment

main :: IO ()
main =
    do args <- getArgs
       case args of
         [portStr] -> launchServer (read portStr)
         _ -> print "./munitweet-server 7000"
