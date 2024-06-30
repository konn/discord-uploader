module Main (handlers, main) where

import Development.GitHub.Discord.Upload.Worker

foreign export javascript "handlers" handlers :: IO JSHandlers

main :: IO ()
main = pure ()
