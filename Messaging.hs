module Messaging where

import Secrets
import Text.Regex.TDFA

-- ------------------------------------------------------------------------------------------------------------------
-- Message
-- The struct dictates what each line will filter into
--    IsPRIVMSG is most important, as that will go on to initiate commands
data Message = UnknownLine String
             | IsPING String
             | IsPRIVMSG { nick :: String
                         , user :: String
                         , host :: String
                         , chan :: String
                         , mess :: String
                         , actv :: Bool  }
    deriving (Show, Read, Eq)

parse :: String -> Message
parse x
    | x =~ "PING :"    = let (_,_,a)       = x =~ "PING :"    :: (String, String, String)
                         in IsPING a
    | x =~ " PRIVMSG " = let (a,_,message) = x =~ " :"        :: (String, String, String)
                             (b,_,channel) = a =~ " PRIVMSG " :: (String, String, String)
                             (c,_,source)  = b =~ "@"         :: (String, String, String)
                             (d,_,usr)     = c =~ "!"         :: (String, String, String)
                             nic           = tail d
                             (_,m,p)       = message =~ ("(^"++botNick++": |^"++attChar++" )") :: (String, String, String)
                         in case null m of
                              True  -> IsPRIVMSG nic usr source channel message False
                              False -> IsPRIVMSG nic usr source channel p True
    | otherwise        = UnknownLine x

-- ------------------------------------------------------------------------------------------------------------------

makeMessage :: String -> String -> String -> String -> String -> String
makeMessage nic usr hst chn mes = ":"++nic++"!"++usr++"@"++hst++" PRIVMSG "++chn++" :"++mes
