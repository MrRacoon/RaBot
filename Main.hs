module Main where

import Driver
import Config
import InitialConfig
import System.Directory
import System.Environment(getArgs)

main = do
  args <- getArgs
  let conf = resolveArguments initialConfig args
      in do
        createDirectoryIfMissing True $ bot_scriptDir conf
        createDirectoryIfMissing True $ bot_logDir conf
        createDirectoryIfMissing True $ bot_commandFile conf
        drive conf


