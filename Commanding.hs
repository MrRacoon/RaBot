module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Actions
import Arguments
import Authorization
import Messaging
import Secrets
import States
import Text.Regex.TDFA
import Triggers


-- ------------------------------------------------------------------------------------------------------------------
-- The Configurable Command structure
-- The commands will essentially take this form in the command file
--    name    -> name of the command, should be unique
--    state   -> Single state for the command
--    auth    -> List of users that may use the command
--    usage   -> A string detailing how to use the command
--    desc    -> A description of what the command does
--    trigger -> A list of triggers in which all must be true for the command
--    action  -> A list of actions that the command will do to fullfill its life
--
data Command = Command { name    :: String
                       , state   :: C_State
                       , auth    :: [ACL]
                       , usage   :: String
                       , desc    :: String
                       , trigger :: [C_Trigger]
                       , action  :: [C_Action] }
    deriving (Show,Read)

--parseCommands :: Message -> [Command] -> Maybe [BotAction]
parseCommands :: Message -> [Command] -> [BotAction]
parseCommands mes com
     = let always  = [SayToTerm (show mes)]
       in (always++) $ case mes of
          (PING h)            -> [SayToServer Raw "" ("PONG :"++h), SayToTerm ("PONGED: "++h)]
          (PRIVMSG a b c d e) -> checkCannonRequest d $ concatMap (tryCommand mes) com
          (JOIN a b c d)      -> [UserAdd d a]
          (PART a b c d)      -> [UserPart d a]
          (QUIT a b c d)      -> [UserQuit a]
          (SERV a b c d)      -> interpretServ mes
          (NICK a b c d)      -> [UserNick a d]
          _        -> []


tryCommand :: Message -> Command -> [BotAction]
tryCommand m@(PRIVMSG nic usr hst chn mes) command
    | copesetic = map (makeAction (PRIVMSG nic usr hst chn messg)) (action command)
    | otherwise = []
  where
    copesetic  = authorized && triggered && stated
    authorized = checkAclList m (auth command)
    triggered  = all (flip trig messg) (trigger command)
    (a,b,c)    = (mes =~ ("(^"++botNick++"[:]? |^"++attChar++" )") :: (String, String, String))
    st         = not $ null b
    messg      = if st then c else a
    stated     = checkState st (state command)


interpretServ (SERV a "353" c d) = let (chan:nics) = words d
                                       list        = words $ tail $ unwords nics
                                   in map (UserAdd chan) list
interpretServ _ = []

-- ------------------------------------------------------------------------------------------------------------------
-- Fileparsing of commands
-- Read in the commands from the designated file

readInCommands :: IO (Maybe [Command])
readInCommands = do
    file <- readFile commandDir
    let clean  = map rmComments $ filter (not . null) $ lines file
        cleanr = unwords $ words $ unwords clean
        coms   = read cleanr :: [Command]
    return $ getCommands cleanr

reloadCommands :: [Command] -> IO (Either [Command] [Command])
reloadCommands last = do
    rel <- readInCommands
    return $ case rel of
      Just new  -> Right new
      Nothing   -> Left last

getCommands :: String -> Maybe [Command]
getCommands = accumulateCommands []

accumulateCommands :: [Command] -> String -> Maybe [Command]
accumulateCommands save []   = Just (save)
accumulateCommands save next = case reads next :: [(Command,String)] of
                                 [(c,[])]  -> Just $ reverse (c:save)
                                 [(c,r)]   -> accumulateCommands (c:save) r
                                 []        -> error next

rmComments []           = []
rmComments ('-':'-':xs) = []
rmComments (x:xs)       = x : (rmComments xs)

