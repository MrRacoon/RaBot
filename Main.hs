module Main where

import Driver
import Config
import System.Directory(createDirectoryIfMissing,doesFileExist)
import System.Environment(getArgs)

--main = do
--  args <- getArgs

main = do
    args <- getArgs
    configExist <- doesFileExist "InitialConfig"
    config <- if configExist
                then readFile "InitialConfig" >>= return . \x -> read x :: BotConfig
                else makeConfig
    return config
    let conf = resolveArguments config args
        in do
          createDirectoryIfMissing True $ bot_scriptDir conf
          createDirectoryIfMissing True $ bot_logDir conf
          createDirectoryIfMissing True $ bot_commandDir conf
          drive conf

