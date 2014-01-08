module Driver where

import Accessory
import Commanding(runCommands)
import Config(BotConfig(..))
import Control.Monad.Trans.State(put,get,StateT(..))
import Messaging(parse)
import Network(connectTo, PortID(..))
import System.IO(hSetBuffering, hGetLine, BufferMode(..))
import Types

-- -----------------------------------------------------------------------------------------------------------------
-- Main Driver
-- Performs the initialization of the bot
--
drive :: BotConfig -> IO (a,BotState)
drive (BotConfig ni at on ou sv pt ch co lo sc dg) = do
  (coms, output) <- loadDirectory co
  mapM_ putStrLn output
  h    <- connectTo sv $ PortNumber $ fromInteger (read pt :: Integer)
  let lobs = map (\x -> (x,[])) ch
      bs   = BotState ni at NoMessage on ou sv pt lobs coms co sc lo [] h dg
  case coms of
    [] -> error "No commands were loaded into the Bot\n"
    _  -> do
          hSetBuffering h NoBuffering
          write h "NICK" ni
          write h "USER" (ni++" 0 * :"++on++"'s bot")
          mapM_ (write h "JOIN") ch
          runStateT listen bs

listen :: StateT BotState IO a
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
