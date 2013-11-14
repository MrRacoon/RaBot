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

tryCommand :: Message -> Command -> Maybe [BotAction]
tryCommand message command
    | copesetic = Just $ map (makeAction message) (action command)
    | otherwise = Nothing
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

data C_State = Active   -- Active state involves the issuer talking directly to the bot
             | Passive  -- Passive state is in affect in all cases where the bot is not directly being instigated
             | Always   -- bot will always attempt to run the command regardless of context
             | Never    -- The bot will never in a million years ever issue the command, it may as well not even exist
    deriving (Show,Read)

checkState _    Never  = False
checkState _    Always = True
checkState True Active = True
checkState _    _      = False

-- ------------------------------------------------------------------------------------------------------------------
-- Triggers
-- How will we know when to execute a given command? One Word: Triggers
--    FirstWord   -> The first word in the message matches a given regex
--    WordPresent -> Some string matching the Regex is present in the message
--
data C_Trigger = FirstWord Regex_Text   -- The first word is <String>
               | WordPresent Regex_Text -- The <String> is present
    deriving (Show,Read)

trig :: C_Trigger -> (String -> Bool)
trig (FirstWord w) = (==w) . head . words
trig (WordPresent w) = (elem w) . words

-- ------------------------------------------------------------------------------------------------------------------
-- Actions
-- The actions are how we will specify the desired action for the command
--    Respond  -> Say something in a channel, the message is dictated by the list of args that will concatenate
--                and the response will land in the channel specified by the Destination
--
data C_Action = Respond [Argument] Destination -- Respond with the string at the channel specified by destination
    deriving (Show,Read)

-- ------------------------------------------------------------------------------------------------------------------
-- Arguments
-- The Arguments Allow us to construct Context Aware Messages that we can output back to the user
--    Literal       -> Stands for the exact string specified
--    WordAfter     -> Replaces itself with the word Directly following the Regex
--    AllWordsAfter -> Is replaced By all of the text that comes after the matching string
--
data Argument = Literal String           -- Stands for the literal string
              | WordAfter Regex_Text     -- return the first word after some regex
              | AllWordsAfter Regex_Text -- Return everything after the Regex
    deriving (Show,Read)

resolveArgs message (Literal s)       = s
resolveArgs message (WordAfter r)     = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in head $ words a
resolveArgs message (AllWordsAfter r) = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in a

-- ------------------------------------------------------------------------------------------------------------------
-- Destination
-- The destination of a Irc Message Message
--    To_Current -> Sends the message to the current window, in which the IRC line originated
--    To_Server  -> Sends a message to the server with no Channel used as a Destination
--
data Destination = To_Current        -- The destination is to the current window in which some essage was recieved
                 | To_Server         -- The destination is no where in particular aside from the server itself RAW
    deriving (Show,Read)

makeDestination message To_Current = chan message
makeDestination _       To_Server  = []

-- ------------------------------------------------------------------------------------------------------------------
-- The BotActions are what the module will strive to return to the calling program
-- Ready to be interpreted and used
--    SayToServer -> Send a line ot the server
--
data BotAction = SayToServer String String
    deriving (Show,Read)

makeAction message (Respond args dest) = SayToServer (makeDestination message dest) (unwords $ map (resolveArgs message) args)

-- ------------------------------------------------------------------------------------------------------------------
-- Fileparsing of commands
-- Read in the commands from the designated file

readInCommands = do
    file <- readFile "Commands"
    let clean  = filter ((/=';') . head) $ filter (not . null) $ lines file
        cleanr = unwords $ words $ unwords clean
        coms   = read cleanr :: [Command]
    return coms

-- ------------------------------------------------------------------------------------------------------------------
-- Examples
commandExample1 = Command { state   = Active
                          , auth    = ["Racoon"]
                          , usage   = "say 'Pst'"
                          , desc    = "Test that the bot is actually working"
                          , trigger = [WordPresent "Pst"]
                          , action  = [Respond [Literal "Hey I heard That"] To_Current ] }



