# RaBot
### Framework for Creating IRC Bot's


## BotState
The BotState is weaved throughout the application, containing most of
the necessities. Configure initial state using the Initial Configuration
File.

```haskell
data BotState
    = BotState 
    { nickname           :: String
      -- The Nickname that the bot Uses in IRC
    , attentionCharacter :: String
      -- The character that the bot will respond to
    , currentMessage     :: Message
      -- Buffer to hlod the current message
    , ownerNick          :: String
      -- Nickname of the bot's owner
    , ownerUser          :: String
      -- Username of the Bot's owner
    , server             :: String
      -- server that the bot is connected to
    , port               :: String
      -- Port the bit uses to connect
    , channels           :: [(String,[String])]
      -- Channel buffer containing all of the connected
      -- channels as well as users in those channels
    , commands           :: [Command]
      -- Command Buffer
    , commandDirectory   :: String
      -- Directory containing the command files
    , scriptDirectory    :: String
      -- Directory Containing scripts to be called from 
      -- commands
    , logsDirectory      :: String
      -- Directory to be the root dir for log files
    , payload            :: [BotAction]
      -- Command buffer
    , handle             :: Handle
      -- handle to sendand recieve messages to and from the
      -- server
    , debug              :: Int }
      -- Level of debug output, I think it goes up to 10
  deriving (Show)
```

## Bot Configuration
Configuration Options for the bot. The bot can be configured either by
file via [InitialConfiguration][InitialConfig] or by specifying command
line arguments. (or both)

```haskell
data BotConfig 
    = BotConfig 
    { bot_nickname    :: String
      -- Name of the bot, and what it will answer to.
    , bot_Char        :: String
      -- Alternate symbol or string the bot will respond to.
    , bot_ownerNick   :: String
      -- Owner of the bot's nickname.
    , bot_ownerUser   :: String
      -- Owner of the bot's username.
    , bot_server      :: String
      -- The server the bot will connect to.
    , bot_port        :: String
      -- Port the bot will use to connect to the server.
    , bot_chans       :: [String]
      -- Initial Channels the bot will join.
    , bot_commandDir  :: String
      -- Directory to read command files from.
    , bot_logDir      :: String
      -- Directory to place logfiles.
    , bot_scriptDir   :: String
      -- Directory to read scripts from.
    , bot_debugLevel  :: Int }
      -- Level of debugging output.
```


## Message
Every IRC Message will be parsed in at least one of the following.

```haskell
data Message
    = PRIVMSG Nick User Host Chan Mess
      -- Private Message
    | JOIN Nick User Host Chan
      -- Join
    | PART Nick User Host Chan
      -- Part
    | QUIT Nick User Host Mess
      -- Quit
    | NICK Nick User Host Nick
      -- Nick Change
    | PING Host
      -- Server Ping
    | SERV Host Code Nick Mess
      -- Server Message
    | UNKNOWN String
      -- UnIdentified Message
    | NoMessage
      -- Void Message
  deriving (Show,Read,Eq)
```


## Commands
The Configurable Command structure
The commands will essentially take this form in the command file

```haskell
data Command
    = Command 
    { name    :: String
      -- Name of the command, should be unique
      --   If the name is left empty, the command will not appear
      --   in help dialogues
    , state   :: C_State
      -- The state of the command
    , auth    :: [ACL]
      -- List of denoting who on IRC may run a command
      -- ACL options explained Below
    , usage   :: String
      -- String example of how the command is used
    , desc    :: String
      -- A description of what the command does
    , trigger :: [C_Trigger]
      -- List of triggers, all of which need to be true in
      -- order for the command to run
    , action  :: [C_Action] }
      -- List of actions that will be run if the command is
      -- triggered and the satet is correct
  deriving (Show,Read)
```


## Actions
The actions are how we will specify the desired action for the command

```haskell
data C_Action
    = KILL
    | Respond Response_Type [Argument] Destination
      -- Say something in a channel, the message is dictated by the list of args that will concatenate
      -- and the response will land in the channel specified by the Destination
    | ReloadCommands
      -- Reload the commands in the command file
    | LogToFile [Argument] [Argument]
    | LoadCannons
    | CheckCannons
    | FireCannons
    | HelpCommandList
      -- List the commands that can be executed by the issuer
    | HelpUsageList
      -- List the usages of commands that can be executed by the issuer
    | HelpDescriptionList
      -- List the descriptions of commands that can be executed by the issuer
    | ShowCurrentUsers
      -- Show the Users in the current channel
    | RunScript String [Argument] Destination
      -- Run a script by the name given as the first param, the second parameter resolves to the
      -- arguments passed to the script and the destination specifies where the output goes
  deriving (Show,Read)
```


## Destination
The destination of an IRC Message Response

```haskell
data Destination
    = To_Current
      -- Output the response to the channel in which the
      -- order was given
    | To_Server
      -- Send the response directly to the server, raw,
      -- with no channel in mind
    | To_Channel String
      -- Send the response to a specific channel denoted by
      -- the string
  deriving (Show,Read)
```


## Message Types
Used for creating bot responses

```haskell
data Response_Type
    = Privmsg
    | Notice
    | Join
    | Part
    | Quit
    | Raw
  deriving (Show,Read,Eq)
```

Arguments
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

```haskell
data Argument
    = NULL
    | SourceUrl
    | Literal String
    | WordAfter String
    | AllWordsAfter String
    | FirstChannelMentioned
    | Message_Nickname
    | Message_Username
    | Message_Channel
    | Message_Hostname
    | Message_WholeMessage
    | Message_AllFields
    | Bot_Nickname
    | Bot_OwnerNick
    | Bot_OwnerUser
    | Bot_AttChar
    | Bot_Server
    | Bot_Port
    | Bot_Channels
    | Bot_CommandCount
    | Bot_CommandDirectory
    | Bot_ScriptDirectory
    | Bot_LogDirectory
    | Bot_PayloadCount
    | Bot_PayloadLoaded
    | Bot_PayloadList
    | Bot_DebugLevel
  deriving (Show,Read)
```


## Command State
Define the context of a command in terms of whether the message targets the bot or not

```haskell
data C_State
    = Active
      -- When the bot is being directly referenced
    | Passive
      -- Bot is not directly referenced
    | Always
      -- Command will always try to run
    | Never
      -- Command will never run, ever. 
  deriving (Show,Read)
```


## Command Authorization
Specify who can execute Commands, all auth tokens specify a user, or group that has sufficient access to execute
the command it is included in. A null list stands for anyone.

```haskell
data ACL 
    = ACL_N
      -- No Authorization is required
    | ACL_W Authorization (match single)
      -- Weak Authorization
    | ACL_M Authorization Authorization
      -- Medium Authorization (match both)
    | ACL_S Authorization Authorization Authorization
      -- Strong Authorization (match all three)
  deriving (Show,Read,Eq)

data Authorization = Auth_Nick String
                     -- Nickname matches sender
                   | Auth_User String
                     -- Username matches sender
                   | Auth_Host String
                     -- Hostname matches sender
    deriving (Show,Read,Eq)
```


## Triggers
The triggers are how commands are triggered from their content

```haskell
data C_Trigger 
    = AllMessages
      -- AllMessages is always true
    | FirstWord String
      -- The first word of the message matches the argument
    | WordPresent String
      -- The argument is present as a word anywhere in the message
    | FollowedBy String String
      -- The first argument is directly followed by the
      -- second in the message
    | EntireMessage String
      -- The entire message matches the argument
    | EmptyMessage
      -- The message is empty
  deriving (Show,Read,Eq)
```


[InitialConf]: https://github.com/MrRacoon/RaBot/InitialConfig.hs.Example
