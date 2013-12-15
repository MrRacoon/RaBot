module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Actions
import Arguments
import Authorization
import Messaging
import States
import Text.Regex.TDFA
import Triggers
import Types



--parseCommands :: Message -> [Command] -> Maybe [BotAction]
parseCommands :: BotState -> Message -> [Command] -> [BotAction]
parseCommands botState mes com
     = let always  = [SayToTerm (show mes)]
       in (always++) $ case mes of
          (PING h)            -> [SayToServer Raw "" ("PONG :"++h), SayToTerm ("PONGED: "++h)]
          (PRIVMSG a b c d e) -> checkCannonRequest d $ concatMap (tryCommand botState mes) com
          (JOIN a b c d)      -> [UserAdd d a]
          (PART a b c d)      -> [UserPart d a]
          (QUIT a b c d)      -> [UserQuit a]
          (SERV a b c d)      -> interpretServ mes
          (NICK a b c d)      -> [UserNick a d]
          _        -> []


tryCommand :: BotState -> Message -> Command -> [BotAction]
tryCommand botState m@(PRIVMSG nic usr hst chn mes) command
    | copesetic = map (makeAction (PRIVMSG nic usr hst chn messg)) (action command)
    | otherwise = []
  where
    copesetic  = authorized && triggered && stated
    authorized = checkAclList m $ (ACL_M (Auth_Nick $ ownerNick botState) (Auth_User $ ownerUser botState)) : auth command
    triggered  = all (flip trig messg) (trigger command)
    (a,b,c)    = (mes =~ ("(^"++(nickname botState)++"[:]? |^"++(attChar botState)++" )") :: (String, String, String))
    st         = not $ null b
    messg      = if st then c else a
    stated     = checkState st (state command)


interpretServ (SERV a "353" c d) = let (chan:nics) = words d
                                       list        = words $ tail $ unwords nics
                                   in map (UserAdd chan) list
interpretServ _ = []

