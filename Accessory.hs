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
  where errString error = "FILE PARSE FAILD: "++file++" on "++(take 99 error)++".."

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

addUser channel name = do
    io $ putStrLn ("Adding user: "++name++"  to channel: "++channel)
    bs <- get
    let ch = channels bs
        x  = addUser' channel name ch
        in put bs { channels = x }
  where
    addUser' c n []  = [(c,[n])]
    addUser' c n ((a,b):xs)
        | c == a     = if elem n b then (a,b) : xs else(a,(n:b)) : xs
        | otherwise  = (a,b) : (addUser' c n xs)

delUser name = do
    io $ putStrLn ("Deleting user: "++name)
    bs <- get
    let ch = channels bs
        x  = delUser' name ch
        in put bs { channels = x }
  where
    delUser' n []     = []
    delUser' n ((a,b):xs) = (a,(b \\ [n])) : delUser' n xs


rmUser channel name = do
    io $ putStrLn ("Removing user: "++name++"  from channel: "++channel)
    bs <- get
    let ch = channels bs
        x  = rmUser' channel name ch
        in put bs { channels = x }
  where
    rmUser' c n [] = []
    rmUser' c n ((a,b):xs)
        | c == a    = (a,(b \\ [n])) : xs
        | otherwise = (a,b) : rmUser' c n xs


getUsers channel = do
    bs <- get
    let ch = channels bs
        x  = getUsers' channel ch
        in return x
  where
    getUsers' _ []   = []
    getUsers' c ((a,b):xs)
        | a == c    = map unPingafy b
        | otherwise = getUsers' c xs

renameUser original new = do
    io $ putStrLn ("Renaming user: "++original++" -> "++new)
    bs <- get
    let ch = channels bs
        x  = renameUser' original new ch
        in put bs { channels = x }
  where
    renameUser' o n []         = []
    renameUser' o n ((a,b):xs)
        | o `elem` b = (a, (n:b) \\ [o]) : renameUser' o n xs
        | otherwise  = (a,b) : renameUser' o n xs

-- ------------------------------------------------------------------------------------------------------------------


unPingafy [] = []
unPingafy (x:xs) = case x of
                      'a' -> '@' : xs
                      'A' -> '4' : xs
                      'e' -> '3' : xs
                      'E' -> '3' : xs
                      'i' -> '1' : xs
                      'I' -> '1' : xs
                      'o' -> '0' : xs
                      'O' -> '0' : xs
                      'l' -> '1' : xs
                      'L' -> '1' : xs
                      't' -> '+' : xs
                      'T' -> '+' : xs
                      's' -> '$' : xs
                      'S' -> '$' : xs
                      'B' -> '|' : '3' : xs
                      x   ->  x  : unPingafy xs

throttle = io $ do
    t <- getCurrentTime
    throttle' t
throttle' x = do
    t <- getCurrentTime
    let diff = diffUTCTime t x
        res  = show diff
    case compare diff (fromRational 1.01) of
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

normalize first second = let width = maximum $ map length first
                             cols  = map (\x -> x ++ (replicate (width - (length x)) ' ') ++ " : ") first
                             in zipWith (++) cols second

debugIt :: Int -> Int -> String -> IO ()
debugIt m n s
    | n <= m = putStrLn $ (++s) $ take n $ repeat '\t'
    | otherwise = return ()

io :: IO a -> Bot a
io = liftIO

