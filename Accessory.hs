module Accessory where

import Types
import Control.Monad.Trans.State(put,get,StateT(..))
import Control.Monad.IO.Class(liftIO)
import Data.Either(lefts,rights)
import Data.List(intersperse, (\\), repeat)
import Data.String.Unicode(unicodeToUtf8)
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import Text.Printf(hPrintf, printf)
import Network(connectTo, PortID(..))
import System.Directory(createDirectoryIfMissing, getDirectoryContents)
import System.Exit(ExitCode(..))
import System.IO(hSetBuffering, hGetLine, Handle(..), BufferMode(..))
import System.Process(readProcessWithExitCode)

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

-- ------------------------------------------------------------------------------------------------------------------
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

runScript bin args chan = do
    bs <- get
    let d = debug bs
        in do
          io $ debugIt d 4 $ "Running Script: "++bin++" "++(unwords args)
          (ec,out,err) <- io $ readProcessWithExitCode bin args []
          let output = map unwords $ map words $ lines out
              errors = map unwords $ map words $ lines ("ERROR OCCURED:\n"++err)
              in case ec of
                  ExitSuccess -> do
                                  io $ debugIt d 4 "Success"
                                  io $ debugIt d 5 out
                                  mapM (say Privmsg chan) output
                                  return ()
                  _           -> do
                                  io $ debugIt d 4 "Failed"
                                  io $ debugIt d 5 err
                                  mapM (say Privmsg chan) errors
                                  return ()

debugIt :: Int -> Int -> String -> IO ()
debugIt m n s
    | n <= m = putStrLn $ (++s) $ take n $ repeat '\t'
    | otherwise = return ()

io :: IO a -> Bot a
io = liftIO

