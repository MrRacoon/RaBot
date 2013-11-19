module Driver where

import Commanding
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.List
import Messaging
import Network
import Secrets
import System.Directory
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
                         , payload  :: [BotAction] }
          deriving (Show)

type Bot = StateT BotState IO

-- -----------------------------------------------------------------------------------------------------------------
-- Main Driver
-- Performs the initialization of the bot
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

-- -----------------------------------------------------------------------------------------------------------------
-- Listen Command that is going to run the bot forever or until completion
--
listen :: Bot a
listen  = forever $ do
      bs <- get
      let h = handle bs
      line <- io $ hGetLine h
      let parsed = parse line
          coms   = parseCommands parsed (commands bs)
      --mapM (io . putStrLn . ("Triggered: "++) . show) coms
      mapM evalCommand coms
  where
      forever a = do a; forever a

-- -----------------------------------------------------------------------------------------------------------------
-- Evaluation
-- Evaluate the commands that are returned from checking the message contents off of the commands held in the bots
-- internal command store
--
evalCommand (SayToServer chan mess) = say chan mess
evalCommand (SayToTerm str)         = io $ putStrLn str
evalCommand (Reload chan)           = do
      bs  <- get
      new <- io $ reloadCommands (commands bs)
      case new of
        Right c -> do say chan "Successfully reloaded :)" >> put (bs {commands = c})
        Left  c -> do say chan "Reload Failed :(" >> put (bs {commands = c})
evalCommand (Log [loc] line) = io $ appendFile (logFolder++"/"++loc) (line++"\n")
evalCommand (Log loc   line) = do
      io $ createDirectoryIfMissing True dir
      io $ appendFile (dir++"/"++file) (line++"\n")
  where
      dir  = (logFolder++) $ concat $ intersperse "/" $ init loc
      file = last loc
evalCommand CannonRequest         = return ()
evalCommand (LoadPayload actions) = do
      bs <- get
      put $ bs {payload = actions}
evalCommand (ShowPayload chan) = do
      bs <- get
      let load = payload bs
      case load of
          []  -> say chan "Payload is currently *Empty*"
          _   -> say chan ("Payload contains *"++(show $ length load)++"* items")
      mapM (say chan . show) load
      return ()
evalCommand (FirePayload) = do
      bs <- get
      mapM evalCommand $ payload bs
      return ()

-- -----------------------------------------------------------------------------------------------------------------
-- IO funtions
-- Varios functions that perform IO functions including io which lifts IO into the Bot monad
--
write :: Handle -> String -> String -> IO ()
write h s t = do
      hPrintf h "%s %s\r\n" s t
      printf    "> %s %s\n" s t


say :: String -> String -> StateT BotState IO b
say chn mes = do
    bs <- get
    let h = handle bs
    case chn of
        [] -> io $ hPrintf h "%s\r\n" mes
        _  -> io $ hPrintf h "PRIVMSG %s :%s\r\n" chn mes

io :: IO a -> Bot a
io = liftIO
