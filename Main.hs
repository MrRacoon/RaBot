module Main where

import Driver
import Config
import Control.Monad(liftM)
import System.Directory(createDirectoryIfMissing,doesFileExist)
import System.Environment(getArgs)
import Types

main :: IO (a,BotState)
main = do
    args <- getArgs
    configExist <- doesFileExist "InitialConfig"
    config <- if configExist
                then liftM (\x -> read x :: BotConfig) (readFile "InitialConfig")
                else makeConfig
    return config
    let conf = resolveArguments config args
        in do
          createDirectoryIfMissing True $ bot_scriptDir conf
          createDirectoryIfMissing True $ bot_logDir conf
          createDirectoryIfMissing True $ bot_commandDir conf
          drive conf

