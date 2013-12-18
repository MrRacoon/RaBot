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
  hSetBuffering h NoBuffering
  write h "NICK" ni
  write h "USER" (ni++" 0 * :"++on++"'s bot")
  mapM (write h "JOIN") ch
  --case coms of
  --  Nothing   -> error "Could not parse the command File...Exiting."
  --  Just coms -> runStateT listen $ BotState ni at on ou sv pt lobs coms co sc lo [] h
  runStateT listen' bs

-- -----------------------------------------------------------------------------------------------------------------
-- Listen Command that is going to run the bot forever or until completion
--
--listen :: Bot a
--listen  = forever $ do
--      bs <- get
--      let h = handle bs
--          in do
--            line <- io $ hGetLine h
--            let parsed = parse line
--                coms   = parseCommands bs parsed (commands bs)
--                in do
--                  io $ putStrLn ""
--                  mapM evalCommand coms
--                  mapM (io . putStrLn . ("Triggered: "++) . show) coms
--  where
--      forever a = do a; forever a

listen'  = forever $ do
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
-- -----------------------------------------------------------------------------------------------------------------
-- Evaluation
-- Evaluate the commands that are returned from checking the message contents off of the commands held in the bots
-- internal command store
--
evalCommand (SayToServer rt chan mess) = say rt chan mess
evalCommand (SayToTerm str)            = io $ putStrLn str
evalCommand (Reload chan)              = do
      bs <- get
      (ers,sucs) <- io $ loadCommandDir $ commandDirectory bs
      mapM (\(f,c) -> say Notice chan $ ("Loaded: "++f)) sucs
      mapM (\(f,e) -> say Notice chan e) ers
      put $ bs { commands = (concatMap snd sucs) }

evalCommand (Log [loc] line) = do 
      bs <- get
      io $ appendFile ((logsDirectory bs)++"/"++loc) (line++"\n")
evalCommand (Log loc line) = do
      bs <- get
      let dir  = ((logsDirectory bs)++) $ concat $ intersperse "/" $ init loc
          file = last loc
          in do
            io $ createDirectoryIfMissing True dir
            io $ appendFile (dir++"/"++file) (line++"\n")
evalCommand CannonRequest         = return ()
evalCommand (LoadPayload actions) = do
      bs <- get
      put $ bs {payload = actions}
evalCommand (ShowPayload chan) = do
      bs <- get
      let load = payload bs
      case load of
          []  -> say Notice chan "Payload is currently *Empty*"
          _   -> say Notice chan ("Payload contains *"++(show $ length load)++"* items")
      mapM (say Notice chan . show) load
      return ()
evalCommand (FirePayload) = do
      bs <- get
      mapM evalCommand $ payload bs
      return ()
evalCommand (DisplayHelp mess typ all) = do
      bs <- get
      let comms = filter (not . null . name) $ commands bs
          owner = ACL_M (Auth_Nick $ ownerNick bs) (Auth_User $ ownerUser bs)
          tshow = case all of
                    True  -> comms
                    False -> filter (\x -> checkAclList mess $ owner : auth x) comms
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
      let lo = channels bs
          ln = addUser c n lo
      put bs {channels = ln}
      return ()
evalCommand (UserPart c n) = do
      bs <- get
      let lo = channels bs
          ln = rmUser c n lo
      put bs {channels = ln}
      return ()
evalCommand (UserQuit n) = do
      bs <- get
      let lo = channels bs
          ln = delUser n lo
      put bs {channels = ln}
      return ()
evalCommand (UserNick o n) = do
      bs <- get
      let lo = channels bs
          ln = renameUser o n lo
      put bs {channels = ln}
      return ()
evalCommand (ShowUsers c) = do
      bs <- get
      let lo = channels bs
          ln = getUsers c lo
      say Privmsg c $ unwords ln
      return ()
evalCommand (Script bin args chan) = do
      bs <- get
      runScript (scriptDirectory bs ++"/"++ bin) args chan
      return ()
evalCommand not_a_command = do
      io $ printf "Could not execute: $s" $ show not_a_command


