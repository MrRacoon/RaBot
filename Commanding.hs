module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Data.List(partition)
import Messaging(IRC(..))
import Text.Regex.TDFA((=~))
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

-- ------------------------------------------------------------------------------------------------------------------
checkAclList  message list   = any (checkAcl message) list
checkAcl message acl =
            case acl of
               ACL_N         -> True
               (ACL_W a)     -> authed message a
               (ACL_M a b)   -> all (authed message) [a,b]
               (ACL_S a b c) -> all (authed message) [a,b,c]
               where
                 authed message (Auth_Nick n) = n == (nick message)
                 authed message (Auth_User u) = u == (user message)
                 authed message (Auth_Host h) = h == (host message)

-- ------------------------------------------------------------------------------------------------------------------

resolveArg :: Message -> Argument -> String
resolveArg _       NULL              = []
resolveArg ms (Literal s)       = s
resolveArg ms (WordAfter r)     = let (_,_,a) = (mess ms) =~ r :: (String, String, String)
                                        in head $ words a
resolveArg ms (AllWordsAfter r) = let (_,_,a) = (mess ms) =~ r :: (String, String, String)
                                        in drop 1 a
resolveArg ms Nickname          = nick ms
resolveArg ms Username          = user ms
resolveArg ms FirstChannel      = (mess ms) =~ "#[^ ]*" :: String
resolveArg ms Channel           = chan ms
resolveArg ms Hostname          = host ms
resolveArg ms WholeMessage      = mess ms
resolveArg ms AllFields         = show ms
resolveArg ms (KarmaUP a)       = (++"++") $ resolveArg ms a
resolveArg ms (KarmaDOWN a)     = (++"--") $ resolveArg ms a
resolveArg _  SourceUrl         = "https://github.com/MrRacoon/RaBot.git"

-- ------------------------------------------------------------------------------------------------------------------

makeAction :: Message -> C_Action -> BotAction
makeAction message (Respond rt args dest)       = SayToServer rt (makeDestination message dest) (unwords $ map (resolveArg message) args)
makeAction message  ReloadCommands              = Reload (chan message)
makeAction message (LogToFile file args)        = Log (map (resolveArg message) file) (concatMap (resolveArg message) args)
makeAction message LoadCannons                  = CannonRequest
makeAction message CheckCannons                 = ShowPayload (chan message)
makeAction message FireCannons                  = FirePayload
makeAction message HelpCommandList              = DisplayHelp message 1 False
makeAction message HelpCommandListAll           = DisplayHelp message 1 True
makeAction message HelpUsageList                = DisplayHelp message 2 False
makeAction message HelpUsageListAll             = DisplayHelp message 2 True
makeAction message HelpDescriptionList          = DisplayHelp message 3 False
makeAction message HelpDescriptionListAll       = DisplayHelp message 3 True
makeAction message ShowCurrentUsers             = ShowUsers (chan message)
makeAction message (RunScript bin args dest)    = Script bin (concatMap words $ map (resolveArg message) args) (makeDestination message dest)

loadable (SayToServer Privmsg _ _) = True
loadable (SayToTerm _)             = True
loadable _                         = False

checkCannonRequest chan actions
      | CannonRequest `elem` actions    = let (load,rest) = partition loadable actions in (LoadPayload load) : rest 
      | otherwise                       = actions


makeDestination :: Message -> Destination -> String
makeDestination message To_Current     = chan message
makeDestination _       To_Server      = []
makeDestination _       (To_Channel s) = s

-- ------------------------------------------------------------------------------------------------------------------

checkState :: Bool -> C_State -> Bool
checkState _     Never   = False
checkState _     Always  = True
checkState True  Active  = True
checkState False Passive = True
checkState _     _       = False

-- ------------------------------------------------------------------------------------------------------------------

trig :: C_Trigger -> String -> Bool
trig EmptyMessage      [] = True
trig _                 [] = False
trig AllMessages       m  = True
trig (FirstWord w)     m  = (w==) $ head $ words m
trig (WordPresent w)   m  = elem w $ words m
trig (EntireMessage r) m  = r == m
trig (FollowedBy a b)  m  = m =~ (" ?"++a++" "++b++"( |$)") :: Bool
trig _                 _  = False
