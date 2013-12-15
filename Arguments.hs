module Arguments where

import Messaging
import Secrets
import Text.Regex.TDFA


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
              | WordAfter String
              | AllWordsAfter String
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
                                        in drop 1 a
resolveArg message Nickname          = nick message
resolveArg message Username          = user message
resolveArg message FirstChannel      = (mess message) =~ "#[^ ]*" :: String
resolveArg message Channel           = chan message
resolveArg message Hostname          = host message
resolveArg message WholeMessage      = mess message
resolveArg message AllFields         = show message
resolveArg message (KarmaUP a)       = (++"++") $ resolveArg message a
resolveArg message (KarmaDOWN a)     = (++"--") $ resolveArg message a
resolveArg _       SourceUrl         = "https://github.com/MrRacoon/RaBot.git"

