module Messaging where

import Text.Regex.TDFA

data Message = UnknownLine String
             | IsPING String
             | IsPRIVMSG { nick :: String
                         , user :: String
                         , host :: String
                         , chan :: String
                         , mess :: String }
    deriving Show

parse :: String -> Message
parse x
    | x =~ "PING :"  = let (_,_,a)       = x =~ "PING :"    :: (String, String, String)
                       in IsPING a
    | x =~ "PRIVMSG" = let (a,_,message) = x =~ " :"        :: (String, String, String)
                           (b,_,channel) = a =~ " PRIVMSG " :: (String, String, String)
                           (c,_,source)  = b =~ "@"         :: (String, String, String)
                           (d,_,usr)     = c =~ "!"         :: (String, String, String)
                           nic           = tail d
                       in IsPRIVMSG nic usr source channel message
    | otherwise      = UnknownLine x


makeMessage nic usr hst chn mes = ":"++nic++"!"++usr++"@"++hst++" PRIVMSG "++chn++" :"++mes
