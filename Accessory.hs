module Accessory where

import Types
import Control.Monad(liftM)
import Control.Monad.Trans.State(put,get,StateT(..))
import Control.Monad.IO.Class(liftIO)
import Data.List((\\))
import Data.String.Unicode(unicodeToUtf8)
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import Text.Printf(hPrintf, printf)
import Messaging
import System.Directory(getDirectoryContents)
import System.Exit(ExitCode(..))
import System.IO(Handle)
import System.Process(readProcessWithExitCode)

-- ------------------------------------------------------------------------------------------------------------------
-- Fileparsing of commands
--
--loadCommandDir path = do
--    files <- getDirectoryContents path
--    let comFiles = filter ((=="sh.") . take 3 . reverse ) $ map ((path++"/")++) files
--        in do
--          comAttempts <- mapM loadCommandsFromFile comFiles
--          let r = rights comAttempts
--              l = lefts comAttempts
--              in return (l,r)
--
--loadCommandsFromFile file = do
--    rel <- readInCommands file
--    case rel of
--      (f,Right new) -> putStrLn ("Loaded Commands From: "++f) >> (return $ Right (f,new))
--      (f, Left err) -> putStrLn (errString err) >> (return $ Left $ (f,errString err))
--  where errString error = "FILE PARSE FAILD: "++file++" on "++(take 99 error)++".."
--
--readInCommands file = do
--    files <- readFile file
--    let clean  = (map rmComments . filter (not . null) . lines) files
--        cleanr = (unwords . words . unwords) clean
--        coms   = getCommands cleanr
--        in return (file, coms)
--  where
--    rmComments []           = []
--    rmComments ('-':'-':xs) = []
--    rmComments (x:xs)       = x : (rmComments xs)
--
--getCommands = accumulateCommands []
--
--accumulateCommands save []   = Right []
--accumulateCommands save next = case reads next :: [(Command,String)] of
--                                 [(c,[])]  -> Right $ reverse (c:save)
--                                 [(c,r)]   -> accumulateCommands (c:save) r
--                                 []        -> Left next
--
-- ------------------------------------------------------------------------------------------------------------------

reloadCommands :: StateT BotState IO ()
reloadCommands = do
  bs <- get
  let comDir = commandDirectory bs
      ch     = chan $ currentMessage bs
      in do
        (comms,output) <- io $ loadDirectory comDir
        put bs { commands = comms }
        io $ mapM putStrLn output
        mapM_ (say Notice ch) output
        return ()

loadDirectory :: Read a => String -> IO ([a], [String])
loadDirectory dir = liftM everything (getFiles >>= mapM readinFile . cleanFilePaths)
  where
    getFiles            = getDirectoryContents dir
    cleanFilePaths      = map ((dir++"/")++) . filter ((=="sh.") . take 3 . reverse )
    everything          = foldr1 (\(a,b) (x,y) -> (a++x, b++y)) . map checkLoad
    checkLoad (f,s,[])  = (s, ["Successfully loaded All Commands from: "++f])
    checkLoad (f,s,e)   = (s, [ "Error loading from: "++f++" :: "++x | x <- e])

readinFile :: Read a => FilePath-> IO (FilePath, [a], [String])
readinFile file = liftM (tackFile . parseFile ([],[]) [] . cleanFileContents) (readFile file)
  where 
    tackFile (a,b)          = (file,a,b)
    cleanFileContents       = unwords . map rmComments . filter (not . null) . lines
    rmComments []           = []
    rmComments ('-':'-':_)  = []
    rmComments xs           = xs


parseFile :: Read a => ([a], [String]) -> String -> String -> ([a], [String])
parseFile (corect,failed) curr str =
  case str of
    []       -> let f = if null curr then failed else reverse curr : failed
                    in (reverse corect, map (unwords . words) $ reverse f)
    a@(x:xs) -> case reads a of
                  []      -> parseFile (corect,failed) (x:curr) xs
                  [(c,r)] -> if null curr 
                               then parseFile (c:corect, failed) curr r
                               else parseFile (c:corect, reverse curr : failed) [] r

-- ------------------------------------------------------------------------------------------------------------------

addUser :: String -> String -> StateT BotState IO ()
addUser channel name' = do
    io $ putStrLn ("Adding user: "++name'++"  to channel: "++channel)
    bs <- get
    let ch = channels bs
        x  = addUser' channel name' ch
        in put bs { channels = x }
  where
    addUser' c n []  = [(c,[n])]
    addUser' c n ((a,b):xs)
        | c == a     = if n `elem` b then (a,b) : xs else(a,n:b) : xs
        | otherwise  = (a,b) : addUser' c n xs

delUser :: String -> StateT BotState IO ()
delUser name' = do
    io $ putStrLn ("Deleting user: "++name')
    bs <- get
    let ch = channels bs
        x  = delUser' name' ch
        in put bs { channels = x }
  where
    delUser' _ []     = []
    delUser' n ((a,b):xs) = (a,b \\ [n]) : delUser' n xs


rmUser :: String -> String -> StateT BotState IO ()
rmUser channel name' = do
    io $ putStrLn ("Removing user: "++name'++"  from channel: "++channel)
    bs <- get
    let ch = channels bs
        x  = rmUser' channel name' ch
        in put bs { channels = x }
  where
    rmUser' _ _ [] = []
    rmUser' c n ((a,b):xs)
        | c == a    = (a,b \\ [n]) : xs
        | otherwise = (a,b) : rmUser' c n xs



getUsers :: String -> StateT BotState IO [String]
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

renameUser :: String -> String -> StateT BotState IO ()
renameUser original new = do
    io $ putStrLn ("Renaming user: "++original++" -> "++new)
    bs <- get
    let ch = channels bs
        x  = renameUser' original new ch
        in put bs { channels = x }
  where
    renameUser' _ _ []         = []
    renameUser' o n ((a,b):xs)
        | o `elem` b = (a, (n:b) \\ [o]) : renameUser' o n xs
        | otherwise  = (a,b) : renameUser' o n xs

-- ------------------------------------------------------------------------------------------------------------------


unPingafy :: String -> String
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
                      a   ->  a  : unPingafy xs

throttle :: Bot ()
throttle = io $ do
    t <- getCurrentTime
    throttle' t
  where
    throttle' x = do
        t <- getCurrentTime
        let diff = diffUTCTime t x
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

runScript :: String -> [String] -> String -> StateT BotState IO ()
runScript bin args chan' = do
    bs <- get
    let d = debug bs
        in do
          io $ debugIt d 4 $ "Running Script: "++bin++" "++unwords args
          (ec,out,err) <- io $ readProcessWithExitCode bin args []
          let output = map (unwords . words) (lines out)
              errors = map (unwords . words) $ lines ("ERROR OCCURED:\n"++err)
              in case ec of
                  ExitSuccess -> do
                                  io $ debugIt d 4 "Success"
                                  io $ debugIt d 5 out
                                  mapM_ (say Privmsg chan') output
                                  return ()
                  _           -> do
                                  io $ debugIt d 4 "Failed"
                                  io $ debugIt d 5 err
                                  mapM_ (say Privmsg chan') errors
                                  return ()

normalize :: [String] -> [String] -> [String]
normalize first second = let width = maximum $ map length first
                             cols  = map (\x -> x ++ replicate (width - length x) ' ' ++ " : ") first
                             in zipWith (++) cols second

debugIt :: Int -> Int -> String -> IO ()
debugIt m n s
    | n <= m = putStrLn $ (++s) $ replicate n '\t'
    | otherwise = return ()

io :: IO a -> Bot a
io = liftIO

