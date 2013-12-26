-- This File outlines the commands that the bot will use during operation
-- You may reload these commands by telling the bot to 'reload'
-- Comented lines begin with a '--'

--
-- Reload all of the commands in this file
-- (This is probably the most important command)
--
Command { name    = "queryNick"
        , state   = Active
        , auth    = []
        , usage   = ">>= state nick"
        , desc    = "display the loaded Nickname"
        , trigger = [FollowedBy "state" "nick"]
        , action  = [Respond Privmsg [Bot_Nickname] To_Current] }

Command { name    = "queryChar"
        , state   = Active
        , auth    = []
        , usage   = ">>= state char"
        , desc    = "display the loaded attention char"
        , trigger = [FollowedBy "state" "char"]
        , action  = [Respond Privmsg [Bot_AttChar] To_Current] }

Command { name    = "queryOwnerNick"
        , state   = Active
        , auth    = []
        , usage   = ">>= state OwnerNick"
        , desc    = "display the loaded Owner Nickname"
        , trigger = [FollowedBy "state" "ownerNick"]
        , action  = [Respond Privmsg [Bot_OwnerNick] To_Current] }

Command { name    = "queryOwnerUser"
        , state   = Active
        , auth    = []
        , usage   = ">>= state OwnerUser"
        , desc    = "display the loaded Owner Username"
        , trigger = [FollowedBy "state" "ownerUser"]
        , action  = [Respond Privmsg [Bot_OwnerUser] To_Current] }

Command { name    = "queryServer"
        , state   = Active
        , auth    = []
        , usage   = ">>= state server"
        , desc    = "display the loaded Server"
        , trigger = [FollowedBy "state" "server"]
        , action  = [Respond Privmsg [Bot_Server] To_Current] }

Command { name    = "queryPort"
        , state   = Active
        , auth    = []
        , usage   = ">>= state port"
        , desc    = "display the loaded Server Port"
        , trigger = [FollowedBy "state" "port"]
        , action  = [Respond Privmsg [Bot_Port] To_Current] }

Command { name    = "queryChannels"
        , state   = Active
        , auth    = []
        , usage   = ">>= state channels"
        , desc    = "display the loaded channels"
        , trigger = [FollowedBy "state" "chan"]
        , action  = [Respond Privmsg [Bot_Channels] To_Current] }

Command { name    = "queryCommandCount"
        , state   = Active
        , auth    = []
        , usage   = ">>= state commandCount"
        , desc    = "display the count of loaded commands"
        , trigger = [FollowedBy "state" "comCount"]
        , action  = [Respond Privmsg [Bot_CommandCount] To_Current] }

Command { name    = "queryCommandDir"
        , state   = Active
        , auth    = []
        , usage   = ">>= state comDir"
        , desc    = "display the directory from which the bot reads commands"
        , trigger = [FollowedBy "state" "comDir"]
        , action  = [Respond Privmsg [Bot_CommandDirectory] To_Current] }

Command { name    = "queryLogDir"
        , state   = Active
        , auth    = []
        , usage   = ">>= state logDir"
        , desc    = "display the directory where the bot keeps logs"
        , trigger = [FollowedBy "state" "logDir"]
        , action  = [Respond Privmsg [Bot_LogDirectory] To_Current] }

Command { name    = "queryScriptDir"
        , state   = Active
        , auth    = []
        , usage   = ">>= state scriptDir"
        , desc    = "display the directory from which the bot loads scripts"
        , trigger = [FollowedBy "state" "scriptDir"]
        , action  = [Respond Privmsg [Bot_ScriptDirectory] To_Current] }

Command { name    = "queryPayloadCount"
        , state   = Active
        , auth    = []
        , usage   = ">>= state payloadCount"
        , desc    = "display the count of commands loaded into the payload"
        , trigger = [FollowedBy "state" "payloadCount"]
        , action  = [Respond Privmsg [Bot_PayloadCount] To_Current] }

Command { name    = "queryPayloadLoaded"
        , state   = Active
        , auth    = []
        , usage   = ">>= state payloadLoaded"
        , desc    = "display the whether or not the bot currently has commands in the payload"
        , trigger = [FollowedBy "state" "payloadLoad"]
        , action  = [Respond Privmsg [Bot_PayloadLoaded] To_Current] }

Command { name    = "queryDebug"
        , state   = Active
        , auth    = []
        , usage   = ">>= state debug"
        , desc    = "display the current level of debug output"
        , trigger = [FollowedBy "state" "debug"]
        , action  = [Respond Privmsg [Bot_DebugLevel] To_Current] }

