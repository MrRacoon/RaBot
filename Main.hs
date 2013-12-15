module Main where

import Driver
import Config
import InitialConfig
import System.Environment(getArgs)

main = do
  args <- getArgs
  drive args


