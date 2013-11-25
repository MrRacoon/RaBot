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
                         , lobbys   :: [(String,[String])]
                         , commands :: [Command]
                         , payload  :: [BotAction] }
          deriving (Show)

type Bot = StateT BotState IO

-- -----------------------------------------------------------------------------------------------------------------
-- Main Driver
-- Performs the initialization of the bot
--
drive = do
      coms <- readInCommands
      h <- connectTo botServer (PortNumber (fromIntegral botPort))
      hSetBuffering h NoBuffering
      write h "NICK" $ botNick
      write h "USER" (botNick++" 0 * :"++botDesc)
      write h "JOIN" initChan
      case coms of
          Nothing   -> error "Could not parse the command File...Exiting."
          Just coms -> runStateT listen $ BotState botServer botPort h initChan botNick [] coms []

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
      io $ putStrLn ""
      mapM evalCommand coms
      mapM (io . putStrLn . ("Triggered: "++) . show) coms
  where
      forever a = do a; forever a

-- -----------------------------------------------------------------------------------------------------------------
-- Evaluation
-- Evaluate the commands that are returned from checking the message contents off of the commands held in the bots
-- internal command store
--
evalCommand (SayToServer rt chan mess) = say rt chan mess
evalCommand (SayToTerm str)            = io $ putStrLn str
evalCommand (Reload chan)              = do
      bs  <- get
      new <- io $ reloadCommands (commands bs)
      case new of
        Right c -> do say Privmsg chan "Successfully reloaded :)" >> put (bs {commands = c})
        Left  c -> do say Privmsg chan "Reload Failed :(" >> put (bs {commands = c})
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
          []  -> say Privmsg chan "Payload is currently *Empty*"
          _   -> say Privmsg chan ("Payload contains *"++(show $ length load)++"* items")
      mapM (say Notice chan . show) load
      return ()
evalCommand (FirePayload) = do
      bs <- get
      mapM evalCommand $ payload bs
      return ()
evalCommand (DisplayHelp mess typ all) = do
      bs <- get
      let comms = filter (not . null . name) $ commands bs
          tshow = case all of
                    True  -> comms
                    False -> filter (\x -> checkAclList mess (auth x)) comms
          msgs  = case typ of
                    1 -> let names = unwords $ map name tshow
                             in [SayToServer Privmsg (chan mess) ("Commands: "++names) ]
                    2 -> map (\x -> SayToServer Privmsg (chan mess) (usage x)) tshow
                    3 -> let names = map name tshow
                             width = maximum $ map length names
                             cols  = map (\x -> x ++ (replicate (width - (length x)) ' ') ++ " : ") names
                             descs = map desc tshow
                              in map (SayToServer Privmsg (chan mess)) (zipWith (++) cols descs)
                    _ -> [Not_a_command]
      mapM evalCommand msgs
      return ()
evalCommand (UserAdd c n) = do
      bs <- get
      let lo = lobbys bs
          ln = addUser c n lo
      put bs {lobbys = ln}
      return ()
evalCommand (UserPart c n) = do
      bs <- get
      let lo = lobbys bs
          ln = rmUser c n lo
      put bs {lobbys = ln}
      return ()
evalCommand (UserQuit n) = do
      bs <- get
      let lo = lobbys bs
          ln = delUser n lo
      put bs {lobbys = ln}
      return ()
evalCommand (UserNick o n) = do
      bs <- get
      let lo = lobbys bs
          ln = renameUser o n lo
      put bs {lobbys = ln}
      return ()
evalCommand (ShowUsers c) = do
      bs <- get
      let lo = lobbys bs
          ln = getUsers c lo
      say Privmsg c $ unwords ln
      return ()
evalCommand not_a_command = do
      io $ printf "Could not execute: $s" $ show not_a_command


delUser n []     = []
delUser n ((a,b):xs) = (a,(b \\ [n])) : delUser n xs

rmUser c n [] = []
rmUser c n ((a,b):xs)
    | c == a    = (a,(b \\ [n])) : xs
    | otherwise = (a,b) : rmUser c n xs

addUser c n []  = [(c,[n])]
addUser c n ((a,b):xs)
    | c == a    = if elem n b then (a,b) : xs else(a,(n:b)) : xs
    | otherwise = (a,b) : (addUser c n xs)

getUsers _ []   = []
getUsers c ((a,b):xs)
    | a == c    = b
    | otherwise = getUsers c xs

renameUser o n []         = []
renameUser o n ((a,b):xs)
    | o `elem` b = (a, (n:b) \\ [o]) : renameUser o n xs
    | otherwise  = (a,b) : renameUser o n xs
-- -----------------------------------------------------------------------------------------------------------------
-- IO funtions
-- Varios functions that perform IO functions including io which lifts IO into the Bot monad
--
write :: Handle -> String -> String -> IO ()
write h s t = do
      hPrintf h "%s %s\r\n" s t
      printf    "> %s %s\n" s t


say :: Response_Type -> String -> String -> StateT BotState IO b
say rt chn mes = do
    bs <- get
    let h = handle bs
    io $ case rt of
        Privmsg -> hPrintf h "PRIVMSG %s :%s\r\n" chn mes
        Notice  -> hPrintf h "NOTICE %s :%s\r\n" chn mes
        Join    -> hPrintf h "JOIN %s\r\n" mes
        Part    -> hPrintf h "PART %s :parting\r\n" mes
        Quit    -> hPrintf h "QUIT :%s\r\n" mes
        _       -> hPrintf h "%s\r\n" mes

notice :: String -> String -> StateT BotState IO b
notice chn mes = do
    bs <- get
    let h = handle bs
    case chn of
        [] -> io $ printf "attempted to notify without specifying channel\n"
        _  -> io $ hPrintf h "NOTICE %s :%s\r\n" chn mes

io :: IO a -> Bot a
io = liftIO
