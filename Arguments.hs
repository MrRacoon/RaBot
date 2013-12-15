module Arguments where

import Messaging
import Text.Regex.TDFA
import Types


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

