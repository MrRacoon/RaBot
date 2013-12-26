module Driver where

import Accessory
import Commanding(checkAclList, runCommands)
import Config(BotConfig(..))
import Control.Monad.Trans.State(put,get,StateT(..))
import Control.Monad.IO.Class(liftIO)
import Data.Either(lefts,rights)
import Data.List(intersperse, (\\))
import Data.String.Unicode(unicodeToUtf8)
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import InitialConfig(initialConfig)
import Messaging(IRC(..), parse)
import Network(connectTo, PortID(..))
import System.Directory(createDirectoryIfMissing, getDirectoryContents)
import System.Exit(ExitCode(..))
import System.IO(hSetBuffering, hGetLine, Handle(..), BufferMode(..))
import System.Process(readProcessWithExitCode)
import Text.Printf(hPrintf, printf)
import Text.Regex.TDFA((=~))
import Types

-- -----------------------------------------------------------------------------------------------------------------
-- Main Driver
-- Performs the initialization of the bot
--
drive (BotConfig ni at on ou sv pt ch co lo sc dg) = do
  (errs,suc) <- loadCommandDir co
  h    <- connectTo sv $ PortNumber $ fromInteger $ (read pt :: Integer)
  let lobs = map (\x -> (x,[])) ch
      coms = concatMap snd suc
      bs   = BotState ni at NoMessage on ou sv pt lobs coms co sc lo [] h dg
  case coms of
    [] -> error "No commands were loaded into the Bot\n"
    c  -> do
          hSetBuffering h NoBuffering
          write h "NICK" ni
          write h "USER" (ni++" 0 * :"++on++"'s bot")
          mapM (write h "JOIN") ch
          runStateT listen bs

listen  = forever $ do
      bs <- get
      let h = handle bs
          in do
            line <- io $ hGetLine h
            let parsed = parse line
                in do
                  put bs { currentMessage = parsed }
                  io $ putStrLn $ ('\n':) $ show parsed
                  runCommands
                  return ()
  where
      forever a = do a; forever a
