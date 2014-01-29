Types
=====

> module Types where

Dependencies
------------

> import System.IO(Handle)
> import Control.Monad.Trans.State(StateT(..)) 

Type Definitions
----------------

> type Bot  = StateT BotState IO
> type Nick = String
> type User = String
> type Host = String
> type Chan = String
> type Mess = String
> type Code = String

BotState
--------
   nickname
     The robots Nickname
   attChar
     Characters that gain the bot's attention
   ownerNick
     Nickname of the owner of the bot
   ownerUser
     Username of the bot owner
   server
     Server that the bot will connect to
   port
     Port on which the bot connects to the server
   lobbys
     Channels, and users in currently joined to the channel
   commands
     The commands that are loaded into the bot
   comFile
     The File the bot will read it the commands from
   logsDir
     Directory for putting logfiles
   payload
     Command buffer to execute commands on demand
   handle
     Connection handle to the IRC server

> data BotState = BotState { nickname           :: String
>                          , attentionCharacter :: String
>                          , currentMessage     :: Message
>                          , ownerNick          :: String
>                          , ownerUser          :: String
>                          , server             :: String
>                          , port               :: String
>                          , channels           :: [(String,[String])]
>                          , commands           :: [Command]
>                          , commandDirectory   :: String
>                          , scriptDirectory    :: String
>                          , logsDirectory      :: String
>                          , payload            :: [String]
>                          , handle             :: Handle
>                          , debug              :: Int }
>           deriving (Show)

Message
-------
Every IRC Message will be parsed in at least one of the following.

> data Message = PRIVMSG Nick User Host Chan Mess
>              | JOIN Nick User Host Chan
>              | PART Nick User Host Chan
>              | QUIT Nick User Host Mess
>              | NICK Nick User Host Nick
>              | PING Host
>              | SERV Host Code Nick Mess
>              | UNKNOWN String
>              | NoMessage
>     deriving (Show,Read,Eq)

Commands
--------
The Configurable Command structure
The commands will essentially take this form in the command file
   name
     Name of the command, should be unique
   state
     Single state for the command
   auth
     List of users that may use the command
   usage
     String detailing how to use the command
   desc
     Description of what the command does
   trigger
     List of triggers in which all must be true for the command
   action
     List of actions that the command will do to fullfill its life

> data Command = Command { name    :: String
>                        , state   :: C_State
>                        , auth    :: [ACL]
>                        , usage   :: String
>                        , desc    :: String
>                        , trigger :: [C_Trigger]
>                        , action  :: [C_Action] }
>     deriving (Show,Read)

Actions
-------
The actions are how we will specify the desired action for the command
   Respond Type [Args] Destination
     Say something in a channel, the message is dictated by the list of args that will concatenate
     and the response will land in the channel specified by the Destination
   ReloadCommands
     reload the commands in the command file
   LogToFile Location Data
     Log to a file specified by the first args, the content specified by the second
   LoadCannons
     Load all other triggered commands into the bot's payload to be fired later
   FireCannons
     Execute the bots payload
   HelpCommandList
     List the commands that can be executed by the issuer
   HelpUsageList
     List the usages of commands that can be executed by the issuer
   HelpDescriptionList
     List the descriptions of commands that can be executed by the issuer
   HelpCommandListAll
     List all commands
   HelpUsageListAll
     List all usages
   HelpDescriptionListAll
     List all descriptions
   ShowCurrentUsers
     Show the Users in the current channel
   RunScript String [Argument] Destination
     Run a script by the name given as the first param, the second parameter resolves to the
     arguments passed to the script and the destination specifies where the output goes

> data C_Action = KILL
>               | Respond Response_Type [Argument] Destination
>               | ReloadCommands
>               | LogToFile [Argument] [Argument]
>               | LoadCannons
>               | CheckCannons
>               | FireCannons
>               | HelpCommandList
>               | HelpUsageList
>               | HelpDescriptionList
>               | ShowCurrentUsers
>               | RunScript String [Argument] Destination
>     deriving (Show,Read)

Destination
-----------
The destination of an Irc Message Message
   To_Current
     Sends the message to the current window, in which the IRC line originated
   To_Server
     Sends a message to the server with no Channel used as a Destination
   To_Channel
     Send the output to a specified channel (or to a queried nick)

> data Destination = To_Current
>                  | To_User
>                  | To_Server
>                  | To_Channel String
>     deriving (Show,Read)

Message Types
-------------

> data Response_Type = Privmsg
>                    | Notice
>                    | Join
>                    | Part
>                    | Quit
>                    | Raw
>     deriving (Show,Read,Eq)

Arguments
---------
The Arguments Allow us to construct Context Aware Messages that we can output back to the user
   NULL
     Null, Return an empty
   Literal
     Stands for the exact string specified
   WordAfter
     Replaces itself with the word Directly following the Regex
   AllWordsAfter
     Is replaced By all of the text that comes after the matching string
   Nickname
     The nicname of the user sending the message
   Username
     Username of the message's source
   FirstChannel
     The first channel that comes appears in the message
   Channel
     The channel that the message is originating
   Hostname
     Hostname that the user is connecting from
   WholeMessage
     The entire message field
   AllFields
     The whole message struct including all fields

> data Argument = NULL
>               | SourceUrl
>               | Literal String
>               | WordAfter String
>               | AllWordsAfter String
>               | FirstChannelMentioned
>               | Message_Nickname
>               | Message_Username
>               | Message_Channel
>               | Message_Hostname
>               | Message_WholeMessage
>               | Message_AllFields
>               | Rainbow Argument
>               | ColorWhite Argument
>               | ColorBlack Argument
>               | ColorBlue Argument
>               | ColorGreen Argument
>               | ColorRed Argument
>               | ColorBrown Argument
>               | ColorPurple Argument
>               | ColorOrange Argument
>               | ColorYellow Argument
>               | ColorLightGreen Argument
>               | ColorCyan Argument
>               | ColorAqua Argument
>               | ColorLightBlue Argument
>               | ColorPink Argument
>               | ColorGrey Argument
>               | ColorSilver Argument
>               | Bot_Nickname
>               | Bot_OwnerNick
>               | Bot_OwnerUser
>               | Bot_AttChar
>               | Bot_Server
>               | Bot_Port
>               | Bot_Channels
>               | Bot_CommandCount
>               | Bot_CommandDirectory
>               | Bot_ScriptDirectory
>               | Bot_LogDirectory
>               | Bot_PayloadCount
>               | Bot_PayloadLoaded
>               | Bot_PayloadList
>               | Bot_DebugLevel
>     deriving (Show,Read)

States
------
Define the context of a command in terms of whether the message targets the bot or not
   Active
     When the Bot is beckoned
   Passive
     When the bot is not directly beckoned
   Always
     For every message in IRC
   Never
     You may as well just forget about ever seeing this command executed

> data C_State = Active
>              | Passive
>              | Always
>              | Never
>     deriving (Show,Read)

Authorization
-------------
Specify who can execute Commands, all auth tokens specify a user, or group that has sufficient access to execute
the command it is included in. A null list stands for anyone.
   Auth_Nick
     Nickname matches sender
   Auth_User
     Username matches sender
   Auth_Host
     Hostname matches sender

> data ACL = ACL_N
>          | ACL_W Authorization
>          | ACL_M Authorization Authorization
>          | ACL_S Authorization Authorization Authorization
>     deriving (Show,Read,Eq)
> 
> data Authorization = Auth_Nick Nick
>                    | Auth_User User
>                    | Auth_Host Host
>     deriving (Show,Read,Eq)

Triggers
--------
How will we know when to execute a given command? One Word: Triggers
   AllMessages
     Triggers command for all messages
   FirstWord
     The first word in the message matches a given regex
   WordPresent
     Some string matching the Regex is present in the message

> data C_Trigger = AllMessages
>                | Anywhere String
>                | FirstWord String
>                | WordPresent String
>                | FollowedBy String String
>                | EntireMessage String
>                | EmptyMessage
>     deriving (Show,Read,Eq)



