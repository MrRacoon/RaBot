-- This File outlines the commands that the bot will use during operation
-- You may reload these commands by telling the bot to 'reload'
-- Comented lines begin with a '--'

--
-- Reload all of the commands in this file
-- (This is probably the most important command)
--
Command { name    = "kill"
        , state   = Active
        , auth    = []
        , usage   = ">>= kill <password>"
        , desc    = "kill the bot instantly, and abrubtly"
        , trigger = [FollowedBy "kill" "pork"]
        , action  = [KILL] }

--
-- Reload all of the commands in this file
-- (This is probably the most important command)
--
Command { name    = "reload"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= reload"
        , desc    = "reload all commands from file"
        , trigger = [WordPresent "reload"]
        , action  = [ReloadCommands] }

--
-- Have the bot join channel
--
Command { name    = "join"
        , state   = Active
        , auth    = []
        , usage   = ">>= join <channel>"
        , desc    = "join a channel"
        , trigger = [WordPresent "join"]
        , action  = [Respond Join [AllWordsAfter "join"] To_Server ] }

--
-- Have the bot leave a channel
--
Command { name    = "leave"
        , state   = Active
        , auth    = []
        , usage   = ">>= leave <channel>"
        , desc    = "leave a channel"
        , trigger = [WordPresent "leave"]
        , action  = [Respond Part [Message_Channel] To_Server ] }

--
-- Have the bot leave a channel
--
Command { name    = "quit"
        , state   = Active
        , auth    = []
        , usage   = ">>= quit"
        , desc    = "leave a channel"
        , trigger = [WordPresent "quit"]
        , action  = [Respond Quit [AllWordsAfter "quit"] To_Server ] }

--
-- Have the bot leave a channel
--
Command { name    = "part"
        , state   = Active
        , auth    = []
        , usage   = ">>=  part <channel>"
        , desc    = "leave a channel"
        , trigger = [WordPresent "part"]
        , action  = [Respond Part [WordAfter "part"] To_Server ] }

Command { name    = "help"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= help"
        , desc    = "Display usage information"
        , trigger = [ FirstWord "help" ]
        , action  = [ HelpUsageList ]}

Command { name    = "list"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= list"
        , desc    = "List possible commands"
        , trigger = [ FirstWord "list" ]
        , action  = [ HelpCommandList ] }

Command { name    = "decribe"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= describe"
        , desc    = "Discribe possible commands"
        , trigger = [ FirstWord "describe" ]
        , action  = [ HelpDescriptionList ]}

Command { name    = "source"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= source"
        , desc    = "Display the URL to the sourceCode"
        , trigger = [ FirstWord "source" ]
        , action  = [ Respond Privmsg [Literal "My source is at:", ColorYellow SourceUrl] To_Current ]}

