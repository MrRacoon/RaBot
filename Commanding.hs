module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Data.List(partition)
import Messaging
import Secrets
import Text.Regex.TDFA

type Regex_Text = String

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
    (a,b,c)    = (mes =~ ("(^"++botNick++" |^"++attChar++" )") :: (String, String, String))
    st         = not $ null b
    messg      = if st then c else a
    stated     = checkState st (state command)


interpretServ (SERV a "353" c d) = let (chan:nics) = words d
                                       list        = words $ tail $ unwords nics
                                   in map (UserAdd chan) list
interpretServ _ = []

-- ------------------------------------------------------------------------------------------------------------------
-- Command States
-- Define the context of a command in terms of whether the message targets the bot or not
--    Active  -> When the Bot is beckoned
--    Passive -> When the bot is not directly beckoned
--    Always  -> For every message in IRC
--    Never   -> You may as well just forget about ever seeing this command executed
--
data C_State = Active
             | Passive
             | Always
             | Never
    deriving (Show,Read)

checkState :: Bool -> C_State -> Bool
checkState _     Never   = False
checkState _     Always  = True
checkState True  Active  = True
checkState False Passive = True
checkState _    _        = False


-- ------------------------------------------------------------------------------------------------------------------
-- Authorization
-- Specify who can execute Commands, all auth tokens specify a user, or group that has sufficient access to execute
-- the command it is included in. A null list stands for anyone.
--    Auth_Nick -> Nickname matches sender
--    Auth_User -> Username matches sender
--    Auth_Host -> Hostname matches sender
--
data ACL = ACL_N
         | ACL_W Authorization
         | ACL_M Authorization Authorization
         | ACL_S Authorization Authorization Authorization
    deriving (Show,Read,Eq)

checkAclList  message []        = checkAcl message (ACL_M (Auth_Nick ownerNick) (Auth_User ownerUser))
checkAclList  message list      = checkAclList' message list
checkAclList' message (x:xs)    = (checkAcl message x) || (checkAclList' message xs)

checkAcl message ACL_N         = True
checkAcl message (ACL_W a)     = (authed message a)
checkAcl message (ACL_M a b)   = (authed message a) && (authed message b)
checkAcl message (ACL_S a b c) = (authed message a) && (authed message b) && (authed message c)

data Authorization = Auth_Nick String
                   | Auth_User String
                   | Auth_Host String
    deriving (Show,Read,Eq)

authed message (Auth_Nick n) = n == (nick message)
authed message (Auth_User u) = u == (user message)
authed message (Auth_Host h) = h == (host message)

-- ------------------------------------------------------------------------------------------------------------------
-- Triggers
-- How will we know when to execute a given command? One Word: Triggers
--    AllMessages -> Triggers command for all messages
--    FirstWord   -> The first word in the message matches a given regex
--    WordPresent -> Some string matching the Regex is present in the message
--
data C_Trigger = AllMessages
               | FirstWord Regex_Text
               | WordPresent Regex_Text
               | EntireMessage Regex_Text
    deriving (Show,Read,Eq)

trig :: C_Trigger -> (String -> Bool)
trig AllMessages      = const True
trig (FirstWord w)    = (==w) . head . words
trig (WordPresent w)  = (elem w) . words
trig (EntireMessage r) = (r==)

-- ------------------------------------------------------------------------------------------------------------------
-- Actions
-- The actions are how we will specify the desired action for the command
--    Respond        -> Say something in a channel, the message is dictated by the list of args that will concatenate
--                      and the response will land in the channel specified by the Destination
--    JoinChannel    -> join the channel specifed
--    ReloadCommands -> reload the commands in the command file
--    LogToFile      -> Log to a file specified by the first args, the content specified by the second
--    LoadCannons    -> Load all commands into the bot's payload to be fired later
--    FireCannons    -> Execute the bots payload
--
data C_Action = Respond Response_Type [Argument] Destination
              | ReloadCommands
              | LogToFile [Argument] [Argument]
              | LoadCannons
              | CheckCannons
              | FireCannons
              | HelpCommandList
              | HelpUsageList
              | HelpDescriptionList
              | HelpCommandListAll
              | HelpUsageListAll
              | HelpDescriptionListAll
              | ShowCurrentUsers
    deriving (Show,Read)

-- ------------------------------------------------------------------------------------------------------------------
-- The BotActions are what the module will strive to return to the calling program
-- Ready to be interpreted and used
--    SayToServer -> Send a line ot the server
--    SayToTerm   -> send some output to the terminal
--    Reload      -> Reload the commands
--    Log         -> log to a file
--
data BotAction = Ping String
               | SayToServer Response_Type String String
               | SayToTerm String
               | Reload String
               | Log [String] String
               | CannonRequest
               | LoadPayload [BotAction]
               | ShowPayload String
               | FirePayload
               | DisplayHelp Message Int Bool
               | UserAdd String String
               | UserQuit String
               | UserPart String String
               | UserNick String String
               | ShowUsers String
               | Not_a_command
    deriving (Show,Read,Eq)

makeAction :: Message -> C_Action -> BotAction
makeAction message (Respond rt args dest)       = SayToServer rt (makeDestination message dest) (unwords $ map (resolveArg message) args)
makeAction message  ReloadCommands              = Reload (chan message)
makeAction message (LogToFile file args)        = Log (map (resolveArg message) file) (concatMap (resolveArg message) args)
makeAction message LoadCannons                  = CannonRequest
makeAction message CheckCannons                 = ShowPayload (chan message)
makeAction message FireCannons                  = FirePayload
makeAction message HelpCommandList              = DisplayHelp message 1 False
makeAction message HelpCommandListAll           = DisplayHelp message 1 True
makeAction message HelpUsageList                = DisplayHelp message 2 False
makeAction message HelpUsageListAll             = DisplayHelp message 2 True
makeAction message HelpDescriptionList          = DisplayHelp message 3 False
makeAction message HelpDescriptionListAll       = DisplayHelp message 3 True
makeAction message ShowCurrentUsers             = ShowUsers (chan message)

loadable (SayToServer Privmsg _ _) = True
loadable (SayToTerm _)             = True
loadable _                         = False

checkCannonRequest chan actions
      | CannonRequest `elem` actions    = let (load,rest) = partition loadable actions in (LoadPayload load) : rest 
      | otherwise                       = actions

-- ------------------------------------------------------------------------------------------------------------------
-- Arguments
-- The Arguments Allow us to construct Context Aware Messages that we can output back to the user
--    NULL          -> Null, Return an empty
--    Literal       -> Stands for the exact string specified
--    WordAfter     -> Replaces itself with the word Directly following the Regex
--    AllWordsAfter -> Is replaced By all of the text that comes after the matching string
--    Nickname      -> The nicname of the user sending the message
--    Username      -> Username of the message's source
--    FirstChannel  -> The first channel that comes appears in the message
--    Channel       -> The channel that the message is originating
--    Hostname      -> Hostname that the user is connecting from
--    WholeMessage  -> The entire message field
--    AllFields     -> The whole message struct including all fields
--
data Argument = NULL
              | Literal String
              | WordAfter Regex_Text
              | AllWordsAfter Regex_Text
              | Nickname
              | Username
              | FirstChannel
              | Channel
              | Hostname
              | WholeMessage
              | AllFields
              | KarmaUP Argument
              | KarmaDOWN Argument
              | SourceUrl
    deriving (Show,Read)

resolveArg :: Message -> Argument -> String
resolveArg _       NULL              = []
resolveArg message (Literal s)       = s
resolveArg message (WordAfter r)     = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in head $ words a
resolveArg message (AllWordsAfter r) = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in tail a
resolveArg message Nickname          = nick message
resolveArg message Username          = user message
resolveArg message FirstChannel      = (mess message) =~ "#[^ ]*" :: String
resolveArg message Channel           = chan message
resolveArg message Hostname          = host message
resolveArg message WholeMessage      = mess message
resolveArg message AllFields         = show message
resolveArg message (KarmaUP a)       = (++"++") $ resolveArg message a
resolveArg message (KarmaDOWN a)     = (++"--") $ resolveArg message a
resolveArg _       SourceUrl         = repo

-- ------------------------------------------------------------------------------------------------------------------
-- Destination
-- The destination of an Irc Message Message
--    To_Current -> Sends the message to the current window, in which the IRC line originated
--    To_Server  -> Sends a message to the server with no Channel used as a Destination
--    To_Channel -> Send the output to a specified channel (or to a queried nick)
--
data Destination = To_Current
                 | To_Server
                 | To_Channel String
    deriving (Show,Read)

makeDestination :: Message -> Destination -> String
makeDestination message To_Current     = chan message
makeDestination _       To_Server      = []
makeDestination _       (To_Channel s) = s

-- ------------------------------------------------------------------------------------------------------------------
-- Message Types
--
data Response_Type = Privmsg
                   | Notice
                   | Join
                   | Part
                   | Quit
                   | Raw
    deriving (Show,Read,Eq)
-- ------------------------------------------------------------------------------------------------------------------
-- Fileparsing of commands
-- Read in the commands from the designated file

readInCommands :: IO (Maybe [Command])
readInCommands = do
    file <- readFile commandFile
    let clean  = filter ((/=';') . head) $ filter (not . null) $ lines file
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

