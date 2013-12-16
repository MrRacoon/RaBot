module Driver where

import Authorization(checkAclList)
import Commanding(parseCommands)
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


-- ---------------------------------------------------------------------
-- dataTypes
--
type Bot = StateT BotState IO

-- -----------------------------------------------------------------------------------------------------------------
-- Main Driver
-- Performs the initialization of the bot
--
drive (BotConfig ni at on ou sv pt ch co lo sc) = do
  (errs,suc) <- loadCommandDir co
  h    <- connectTo sv $ PortNumber $ fromInteger $ (read pt :: Integer)
  let lobs = map (\x -> (x,[])) ch
      coms = concatMap snd suc
      bs   = BotState ni at on ou sv pt lobs coms co sc lo [] h
  hSetBuffering h NoBuffering
  write h "NICK" ni
  write h "USER" (ni++" 0 * :"++on++"'s bot")
  mapM (write h "JOIN") ch
  --case coms of
  --  Nothing   -> error "Could not parse the command File...Exiting."
  --  Just coms -> runStateT listen $ BotState ni at on ou sv pt lobs coms co sc lo [] h
  runStateT listen bs

-- ------------------------------------------------------------------------------------------------------------------
-- Fileparsing of commands
--
-- Read in the commands from the designated file

loadCommandDir path = do
    files <- getDirectoryContents path
    let comFiles = filter ((=="sh.") . take 3 . reverse ) $ map ((path++"/")++) files
        in do
          comAttempts <- mapM loadCommandsFromFile comFiles
          let r = rights comAttempts
              l = lefts comAttempts
              in return (l,r)

loadCommandsFromFile file = do
    rel <- readInCommands file
    case rel of
      (f,Right new) -> putStrLn ("Loaded Commands From: "++f) >> (return $ Right (f,new))
      (f, Left err) -> putStrLn (errString err) >> (return $ Left $ (f,errString err))
  where errString error = "FILE PARSE FAILD: "++file++" on "++(take 30 error)++".."

readInCommands file = do
    files <- readFile file
    let clean  = (map rmComments . filter (not . null) . lines) files
        cleanr = (unwords . words . unwords) clean
        coms   = getCommands cleanr
        in return (file, coms)
  where
    rmComments []           = []
    rmComments ('-':'-':xs) = []
    rmComments (x:xs)       = x : (rmComments xs)

getCommands = accumulateCommands []

accumulateCommands save []   = Right []
accumulateCommands save next = case reads next :: [(Command,String)] of
                                 [(c,[])]  -> Right $ reverse (c:save)
                                 [(c,r)]   -> accumulateCommands (c:save) r
                                 []        -> Left next


{-
reload = do
  bs <- get
  ne <- map readInCommands $ commandFile bs

-}
-- -----------------------------------------------------------------------------------------------------------------
-- Listen Command that is going to run the bot forever or until completion
--
listen :: Bot a
listen  = forever $ do
      bs <- get
      let h = handle bs
          in do
            line <- io $ hGetLine h
            let parsed = parse line
                coms   = parseCommands bs parsed (commands bs)
                in do
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
      bs <- get
      (ers,sucs) <- io $ loadCommandDir $ comDir bs
      mapM (\(f,c) -> say Notice chan $ ("Loaded: "++f)) sucs
      mapM (\(f,e) -> say Notice chan e) ers
      put $ bs { commands = (concatMap snd sucs) }

evalCommand (Log [loc] line) = do 
      bs <- get
      io $ appendFile ((logsDir bs)++"/"++loc) (line++"\n")
evalCommand (Log loc line) = do
      bs <- get
      let dir  = ((logsDir bs)++) $ concat $ intersperse "/" $ init loc
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
evalCommand (Script bin args chan) = do
      bs <- get
      runScript (scptDir bs ++"/"++ bin) args chan
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
    | a == c    = map unPingafy b
    | otherwise = getUsers c xs

renameUser o n []         = []
renameUser o n ((a,b):xs)
    | o `elem` b = (a, (n:b) \\ [o]) : renameUser o n xs
    | otherwise  = (a,b) : renameUser o n xs

unPingafy []       = []
unPingafy ('a':xs) = '@' : unPingafy xs
unPingafy ('A':xs) = '4' : unPingafy xs
unPingafy ('e':xs) = '3' : unPingafy xs
unPingafy ('E':xs) = '3' : unPingafy xs
unPingafy ('i':xs) = '1' : unPingafy xs
unPingafy ('I':xs) = '1' : unPingafy xs
unPingafy ('o':xs) = '0' : unPingafy xs
unPingafy ('O':xs) = '0' : unPingafy xs
unPingafy ('l':xs) = '1' : unPingafy xs
unPingafy ('L':xs) = '1' : unPingafy xs
unPingafy (x:xs)   = x : unPingafy xs

throttle = io $ do
    t <- getCurrentTime
    throttle' t
throttle' x = do
    t <- getCurrentTime
    let diff = diffUTCTime t x
        res  = show diff
    case compare diff (fromRational 0.5) of
         LT -> throttle' x
         _  -> return ()

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
        m = unicodeToUtf8 mes
    throttle
    io $ case rt of
        Privmsg -> hPrintf h "PRIVMSG %s :%s\r\n" chn m
        Notice  -> hPrintf h "NOTICE %s :%s\r\n" chn m
        Join    -> hPrintf h "JOIN %s\r\n" m
        Part    -> hPrintf h "PART %s :parting\r\n" m
        Quit    -> hPrintf h "QUIT :%s\r\n" m
        _       -> hPrintf h "%s\r\n" m

notice :: String -> String -> StateT BotState IO b
notice chn mes = do
    bs <- get
    let h = handle bs
    case chn of
        [] -> io $ printf "attempted to notify without specifying channel\n"
        _  -> io $ hPrintf h "NOTICE %s :%s\r\n" chn mes

runScript bin args chan = do
    (ec,out,err) <- io $ readProcessWithExitCode bin args []
    let output = map unwords $ map words $ lines out
        errors = map unwords $ map words $ lines ("ERROR OCCURED:\n"++err)
    case ec of
        ExitSuccess -> (mapM (say Privmsg chan) output) >> return ()
        _           -> mapM (say Privmsg chan) errors >> return ()

trimOut x = trimOutput ([],x)
trimOutput ([],[]) = []
trimOutput (x,[]) = [x]
trimOutput ([],x) = trimOutput $ splitAt 60 x
trimOutput (x,xs) = x : (trimOutput $ splitAt 60 x)


io :: IO a -> Bot a
io = liftIO
