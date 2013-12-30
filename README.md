# RaBot
### Modular IRC Bot Framework

I wrote this framework, while working at the Computer Action Team at
Portland State University. Mainly for fun and practice. 

The idea is that all of the commands get loaded from the command
directory at runtime, and can be reloaded at anytime in the bot's
lifespan. The syntax adheres to typical Haskell syntax.

## Make a Bot

Use your package manager of choice to install the haskell platform if
you don't already have it. The platform includes Haskell's awesome
module manager `cabal` which we'll need later. The following example is
for use on debian systems. 

`sudo apt-get install haskell-platform`

Now, RaBot requires two modules to run, one for regex, and the other to
handle Unicode -> utf8. Use `cabal` to install the following packages
like so.

```bash
cabal update
cabal install regex-tdfa
cabal install hxt-unicode
```

Now we're set to build RaBot!

Get the source code from GitHub.

`git clone https://github.com/MrRacoon/RaBot`

compile and go!

```bash
cd RaBot
ghc --make Main.hs -o RaBot
./RaBot
```

## Commands

```haskell
data Command  
    = Command   
    { name    :: String   
    , state   :: C_State   
    , auth    :: [ACL]
    , usage   :: String
    , desc    :: String
    , trigger :: [C_Trigger]
    , action  :: [C_Action] }
  deriving (Show,Read)
```

### Name
The name of the command comes into play whenever command help is 
requested. If this field is ommited using the empty string, `""` help 
information will not be displayed.

### State
The `State` is going to specify when a `Command` needs to be run
depending on whether the bot is being requested directly or not.

  * `Active`
    * Commands that are denoted as active will only get run if the
      commander is talking directly to the bot, either prefixing the
      message with the bot's name followed by a colon, the attention
      char, or by talking to the bot in a Query message.
  * `Passive`
    * Activates the command only if the message *is* *not* talking
      directly to the bot in an ACtive way. 
  * `Always`
    * The command is always in effect and will be run if the the user is
      authorized and the triggers are true.
  * `Never`
    * No matter what you do to this bot, the `Never` state disallows the
      command from ever being run.

### Auth
In order to ensure that only the people you want executing certain
commands, are the only people executing commnds, there is a basic
authorization framework. 

#### Authorization Levels
```haskell
data ACL 
    = ACL_N
    | ACL_W Authorization 
    | ACL_M Authorization Authorization
    | ACL_S Authorization Authorization Authorization
  deriving (Show,Read,Eq)
```
  * `ACL_N`
    * Anyone can issue this command
  * `ACL_W Token`
    * Weak Auth requiring a single token
  * `ACL_M Token Token`
    * Medium Security Authorization requiring two tokens
  * `ACL_S Token Token Token`
    * Strongest Aith requiring three tokens

#### Authorization Token Types
```haskell
data Authorization 
    = Auth_Nick String
    | Auth_User String
    | Auth_Host String
  deriving (Show,Read,Eq)
```
  * `AUTH_Nick String`
    * Matches the Nickname of the user from which the message originates
  * `AUTH_User String`
    * Matches the username of the user from which the message originates
  * `AUTH_Host String`
    * Matches the hostname of the user from which the message originates


### Usage
Usage is a string denoting how the command should be used. The
description only ever crops up when the commander asks for help. 

### Desc
The description is a description of the command and like the usage field
only ever crops up when the commander requests for a description of the
various commands.

### Trigger
The triggers are used to trigger a command based off the content of the
message field in a message over IRC.

```haskell
data C_Trigger 
    = AllMessages
    | FirstWord String
    | WordPresent String
    | FollowedBy String String
    | EntireMessage String
    | EmptyMessage
  deriving (Show,Read,Eq)
```
  * `AllMessages`
     * AllMessages is always true
  * `FirstWord String`
     * The first word of the message matches the argument
  * `WordPresent String`
     * The argument is present as a word anywhere in the message
  * `FollowedBy String String`
     * The first argument is directly followed by the
       second in the message
  * `EntireMessage String`
     * The entire message matches the argument
  * `EmptyMessage`
     * The message is empty
 
### Action
Actions are run for every `Command` that is triggered successfully.

```haskell
data C_Action
    = KILL
    | Respond Response_Type [Argument] Destination
    | ReloadCommands
    | LogToFile [Argument] [Argument]
    | HelpCommandList
    | HelpUsageList
    | HelpDescriptionList
    | RunScript String [Argument] Destination
  deriving (Show,Read)
```

  * `KILL`
  * `Respond Response_Type [Argument] Destination`
     * Say something in a channel, the message is dictated by the list of args that will concatenate
       and the response will land in the channel specified by the Destination
  * `ReloadCommands`
     * Reload the commands in the command file
  * `LogToFile [Argument] [Argument]`
  * `HelpCommandList`
     * List the commands that can be executed by the issuer
  * `HelpUsageList`
     * List the usages of commands that can be executed by the issuer
  * `HelpDescriptionList`
     * List the descriptions of commands that can be executed by the issuer
  * `RunScript String [Argument] Destination`
     * Run a script by the name given as the first param, the second parameter resolves to the
       arguments passed to the script and the destination specifies where the output goes


#### Arguments
Arguments are tokens that will resolve to strings based on the state of
the bot at the given time in which they are resolved.

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
    | Bot_DebugLevel
  deriving (Show,Read)
```

  * `NULL`
    * the absence of information, returns `[]`.
  * `SourceUrl`
    * The Url in which the bot's source can be found.
  * `Literal String`
    * Uses the provided `String` literally.
  * `WordAfter String`
    * Returns the first word after the provided string in the users 
      message or `[]` if the word is not found.
  * `AllWordsAfter String`
    * Returns the rest of the message after the first instance of the
      word from the message, or `[]` if not present.
  * `FirstChannelMentioned`
    * The first word prepended with a `#` character.
  * `Message_Nickname`
    * Nickname of the user from which the message originates.
  * `Message_Username`
    * Username of the user from which the message originates.
  * `Message_Channel`
    * Channel from which the message was retrieved from.
  * `Message_Hostname`
    * The hostname from which the meassage originated from.
  * `Message_WholeMessage`
    * The entire contents of the message.
  * `Message_AllFields`
    * The entire Message object as a string, including usr/nic/host/chan
      fields
  * `Bot_Nickname`
    * Nickname stored in the bot's state.
  * `Bot_OwnerNick`
    * The IRC handle of the bot's owner.
  * `Bot_OwnerUser`
    * The username the Bot' owner uses to connect to IRC.
  * `Bot_AttChar`
    * The character that the bot will respond to for `Active` comands
      along with it's own nickname.
  * `Bot_Server`
    * The IRC server address that the bot is connected to.
  * `Bot_Port`
    * The port the bot uses to connect to the server.
  * `Bot_Channels`
    * All of the channels that the bot is currently connected to.
  * `Bot_CommandCount`
    * The number of commands the bot has loaded in the command buffer.
  * `Bot_CommandDirectory`
    * The directory from which the bot loads its commands.
  * `Bot_ScriptDirectory`
    * The directory from which the bot loads scripts for execution.
  * `Bot_LogDirectory`
    * The root directory where the bot will save logging command output.
  * `Bot_DebugLevel`
    * The level of debugging output the bot is giving.

#### Destination
The destination of an IRC Message Response

```haskell
data Destination
    = To_Current
    | To_Server
    | To_Channel String
  deriving (Show,Read)
```

  * `To_Current`
    * Output the response to the channel in which the
      order was given
  * `To_Server`
    * Send the response directly to the server, raw,
      with no channel in mind
  * `To_Channel String`
    * Send the response to a specific channel denoted by
      the string

