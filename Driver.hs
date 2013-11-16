module Driver where

import Commanding
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.List
import Messaging
import Network
import Secrets
import System.IO
import Text.Printf
import Text.Regex.TDFA


-- ---------------------------------------------------------------------
  -- dataTypes
  --

data BotState = BotState { server   :: String
                         , port     :: Integer
                         , handle   :: Handle
                         , channels :: String
                         , nickname :: String
                         , masters  :: [String]
                         , commands :: [Command]
                         , buffer   :: String }
          deriving (Show)

type Bot = StateT BotState IO

-- ---------------------------------------------------------------------
  -- Drivers
  --
main = do
      coms <- readInCommands
      h <- connectTo botServer (PortNumber (fromIntegral botPort))
      hSetBuffering h NoBuffering
      write h "NICK" botNick
      write h "USER" (botNick++" 0 * :"++botDesc)
      write h "JOIN" initChan
      case coms of
          Nothing   -> error "Could not parse the command File...Exiting."
          Just coms -> runStateT listen $ BotState botServer botPort h initChan botNick initMasters coms []

write :: Handle -> String -> String -> IO ()
write h s t = do
      hPrintf h "%s %s\r\n" s t
      printf    "> %s %s\n" s t

listen :: Bot a
listen  = forever $ do
      bs <- get
      let h = handle bs
      line <- io $ hGetLine h
      let parsed = parse line
          coms   = parseCommands parsed (commands bs)
      mapM evalCommand coms
  where
      forever a = do a; forever a

evalCommand (SayToServer chan mess) = say chan mess
evalCommand (SayToTerm str)         = io $ putStrLn str
evalCommand (Reload chan)           = do
      bs  <- get
      new <- io $ reloadCommands (commands bs)
      case new of
        Right c -> do say chan "Successfully reloaded :)" >> put (bs {commands = c})
        Left  c -> do say chan "Reload Failed :(" >> put (bs {commands = c})

say :: String -> String -> StateT BotState IO b
say chn mes = do
    bs <- get
    let h = handle bs
    case chn of
        [] -> io $ hPrintf h "%s\r\n" mes
        _  -> io $ hPrintf h "PRIVMSG %s :%s\r\n" chn mes

io :: IO a -> Bot a
io = liftIO
