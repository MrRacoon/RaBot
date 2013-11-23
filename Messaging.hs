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
    | x =~ " PRIVMSG " = let (a,_,mes_field) = x =~ " :"        :: (String, String, String)
                             (b,_,chn_field) = a =~ " PRIVMSG " :: (String, String, String)
                             (c,_,hst_field) = b =~ "@"         :: (String, String, String)
                             (d,_,usr_field) = c =~ "!"         :: (String, String, String)
                             nic_field       = tail d
                             (_,m,p)         = mes_field =~ ("(^"++botNick++": |^"++attChar++" )") :: (String, String, String)
                             active          = ((chn_field == botNick) || (not $ null m))
                             chan            = if chn_field == botNick then nic_field else chn_field
                             mess            = if null m then mes_field else p
                         in IsPRIVMSG nic_field usr_field hst_field chan mess active
    | otherwise        = UnknownLine x

-- ------------------------------------------------------------------------------------------------------------------

makeMessage :: String -> String -> String -> String -> String -> String
makeMessage nic usr hst chn mes = ":"++nic++"!"++usr++"@"++hst++" PRIVMSG "++chn++" :"++mes
