module Actions where

import Arguments
import Data.List(partition)
import Messaging
import Secrets


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
