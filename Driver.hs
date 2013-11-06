module Driver where

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
                         , buffer   :: String }
          deriving (Show)

type Bot = StateT BotState IO

-- ---------------------------------------------------------------------
  -- Drivers
  --
main = do
      h <- connectTo botServer (PortNumber (fromIntegral botPort))
      hSetBuffering h NoBuffering
      write h "NICK" botNick
      write h "USER" (botNick++" 0 * :"++botDesc)
      write h "JOIN" initChan
      let bot = BotState botServer botPort h initChan botNick initMasters []
      runStateT listen bot

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
      case parsed of
        IsPING s                      -> io $ write h "PONG" s
        IsPRIVMSG nic usr hst chn mes -> eval nic usr hst chn mes
        UnknownLine l                 -> io $ putStrLn $ ("NOTHING: "++l)
  where
      forever a = do a; forever a

eval nic usr hst chn mes = do
    bs <- get
    let (h,b,m) = (handle bs, buffer bs,masters bs)
        auth    = elem nic m
        sameChn = "PRIVMSG "++chn++ " :"
        (x:xs)  = words mes
    case (auth,x) of
      (True, "join")      -> if null xs
                              then say chn "No Channels provided for join"
                              else let chan = head xs
                                       key  = if null $ tail xs
                                                then []
                                                else head $ tail xs
                                   in do
                                     say chn ("Joining Channel "++chan)
                                     io $ write h "JOIN" (unwords [chan,key])
      (True, "part")      -> io $ write h ("PART "++chn) []
      (True, "quit")      -> io $ write h "QUIT" "Quitting per order of: "
      (True, "put")       -> let newBuff = b ++ (unwords xs) in do put (bs { buffer = newBuff}) >> (io $ write h sameChn newBuff)
      (True, "show")      -> io $ write h sameChn b
      (True, "delete")    -> do put (bs {buffer = []} ) >> (io $ write h sameChn "Buffer empty")
      (True, "addMaster") -> if null xs
                        then io $ write h sameChn "no args" 
                        else let newMast = (head xs)
                                 newList = newMast : m
                             in do
                               put (bs { masters = newList })
                               io $ write h sameChn ("Added Master "++newMast)
      (_,"auth")          -> say chn $ unwords m
      (_,"help")          -> io $ write h sameChn "who are you to ask me for help"
      _                   -> io $ putStrLn ("Message: "++(makeMessage nic usr hst chn mes))

say :: String -> String -> StateT BotState IO b
say chn mes = do
    bs <- get
    let h = handle bs
    io $ printf    "> %s :%s\n" chn mes
    io $ hPrintf h "PRIVMSG %s :%s\r\n" chn mes

io :: IO a -> Bot a
io = liftIO
