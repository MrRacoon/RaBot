module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Messaging
import Secrets
import Text.Regex.TDFA

type Regex_Text = String

-- ------------------------------------------------------------------------------------------------------------------
-- The Configurable Command structure
-- The commands will essentially take this form in the command file
--    state   -> Single state for the command
--    auth    -> List of users that may use the command
--    usage   -> A string detailing how to use the command
--    desc    -> A description of what the command does
--    trigger -> A list of triggers in which all must be true for the command
--    action  -> A list of actions that the command will do to fullfill its life
--
data Command = Command { state   :: C_State      -- state that the command is in
                       , auth    :: [String]     -- People with authority to issue the command
                       , usage   :: String       -- How to use the command
                       , desc    :: String       -- Description of the command
                       , trigger :: [C_Trigger]  -- How to trigger the command
                       , action  :: [C_Action] } -- What the command will do
    deriving (Show,Read)

--parseCommands :: Message -> [Command] -> Maybe [BotAction]
parseCommands (IsPING server) = const [SayToServer "" ("PONG :"++server), SayToTerm ("Ponged: "++server)]
parseCommands (UnknownLine l) = const [SayToTerm l]
parseCommands mess = ([SayToTerm (show mess)] ++) . concatMap (tryCommand mess)


tryCommand :: Message -> Command -> [BotAction]
tryCommand (IsPING server) _ = [SayToServer "" ("PONG"++server)]
tryCommand message command
    | copesetic = map (makeAction message) (action command)
    | otherwise = []
  where
    copesetic  = authorized && triggered && stated
    authorized = (nick message) `elem` (auth command) || null (auth command) || (nick message) == owner
    triggered  = all (flip trig (mess message)) (trigger command)
    stated     = checkState (actv message) (state command)

-- ------------------------------------------------------------------------------------------------------------------
-- Command States
--    Active  -> When the Bot is beckoned
--    Passive -> When the bot is not directly beckoned
--    Always  -> For every message in IRC
--    Never   -> You may as well just forget about ever seeing this command executed
--
data C_State = Active   -- Active state involves the issuer talking directly to the bot
             | Passive  -- Passive state is in affect in all cases where the bot is not directly being instigated
             | Always   -- bot will always attempt to run the command regardless of context
             | Never    -- The bot will never in a million years ever issue the command, it may as well not even exist
    deriving (Show,Read)

checkState :: Bool -> C_State -> Bool
checkState _     Never   = False
checkState _     Always  = True
checkState True  Active  = True
checkState False Passive = True
checkState _    _        = False

-- ------------------------------------------------------------------------------------------------------------------
-- Triggers
-- How will we know when to execute a given command? One Word: Triggers
--    AllMessages -> Triggers command for all messages
--    FirstWord   -> The first word in the message matches a given regex
--    WordPresent -> Some string matching the Regex is present in the message
--
data C_Trigger = AllMessages            -- trigger is true on all messages
               | FirstWord Regex_Text   -- The first word is <String>
               | WordPresent Regex_Text -- The <String> is present
    deriving (Show,Read)

trig :: C_Trigger -> (String -> Bool)
trig AllMessages     = const True
trig (FirstWord w)   = (==w) . head . words
trig (WordPresent w) = (elem w) . words

-- ------------------------------------------------------------------------------------------------------------------
-- Actions
-- The actions are how we will specify the desired action for the command
--    Respond        -> Say something in a channel, the message is dictated by the list of args that will concatenate
--                      and the response will land in the channel specified by the Destination
--    JoinChannel    -> join the channel specifed
--    ReloadCommands -> reload the commands in the command file
--    LogToFile      -> Log to a file specified by the first args, the content specified by the second
--
data C_Action = Respond [Argument] Destination             -- Respond with the string at the channel specified by destination
              | JoinChannel Argument Argument              -- Join the matched Channel with the matched Key
              | ReloadCommands                             -- Command to Reload the commands of the Bot
              | LogToFile [Argument] [Argument]            -- Log arguments to a file
    deriving (Show,Read)

-- ------------------------------------------------------------------------------------------------------------------
-- The BotActions are what the module will strive to return to the calling program
-- Ready to be interpreted and used
--    SayToServer -> Send a line ot the server
--    SayToTerm   -> send some output to the terminal
--    Reload      -> Reload the commands
--    Log         -> log to a file
--
data BotAction = SayToServer String String
               | SayToTerm String
               | Reload String
               | Log [String] String
    deriving (Show,Read)

makeAction :: Message -> C_Action -> BotAction
makeAction message (Respond args dest)          = SayToServer (makeDestination message dest) (unwords $ map (resolveArg message) args)
makeAction message (JoinChannel ch k)           = SayToServer "" (unwords ["JOIN",(resolveArg message ch),(resolveArg message k)])
makeAction message  ReloadCommands              = Reload (chan message)
makeAction message (LogToFile file args)        = Log (map (resolveArg message) file) (concatMap (resolveArg message) args)

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
data Argument = NULL                     -- For those actions that take optional arguments
              | Literal String           -- Stands for the literal string
              | WordAfter Regex_Text     -- return the first word after some regex
              | AllWordsAfter Regex_Text -- Return everything after the Regex
              | Nickname                 -- resolve the Nickname of the sender
              | Username                 -- Resolve the Username of the user
              | FirstChannel             -- Match the first channel in the message
              | Channel                  -- Return the current Channel
              | Hostname                 -- The hostname of the user
              | WholeMessage             -- The entire message section
              | AllFields                -- The entire message including the stats
    deriving (Show,Read)

resolveArg :: Message -> Argument -> String
resolveArg _       NULL              = []
resolveArg message (Literal s)       = s
resolveArg message (WordAfter r)     = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in head $ words a
resolveArg message (AllWordsAfter r) = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in a
resolveArg message Nickname          = nick message
resolveArg message Username          = user message
resolveArg message FirstChannel      = (mess message) =~ "#[^ ]*" :: String
resolveArg message Channel           = chan message
resolveArg message Hostname          = host message
resolveArg message WholeMessage      = mess message
resolveArg message AllFields         = show message

-- ------------------------------------------------------------------------------------------------------------------
-- Destination
-- The destination of an Irc Message Message
--    To_Current -> Sends the message to the current window, in which the IRC line originated
--    To_Server  -> Sends a message to the server with no Channel used as a Destination
--    To_Channel -> Send the output to a specified channel (or to a queried nick)
--
data Destination = To_Current        -- current window in which some essage was recieved
                 | To_Server         -- no where in particular aside from the server itself RAW
                 | To_Channel String -- A specific destination
    deriving (Show,Read)

makeDestination :: Message -> Destination -> String
makeDestination message To_Current     = chan message
makeDestination _       To_Server      = []
makeDestination _       (To_Channel s) = s

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
                                 [(c,[])]  -> Just (c:save)
                                 [(c,r)]   -> accumulateCommands (c:save) r
                                 []        -> Nothing

-- ------------------------------------------------------------------------------------------------------------------
-- Examples
commandExample1 :: Command
commandExample1 = Command { state   = Active
                          , auth    = ["Racoon"]
                          , usage   = "say 'Pst'"
                          , desc    = "Test that the bot is actually working"
                          , trigger = [WordPresent "Pst"]
                          , action  = [Respond [Literal "Hey I heard That"] To_Current ] }



