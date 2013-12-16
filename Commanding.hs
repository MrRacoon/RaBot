module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Control.Monad.Trans.State(get,put)
import Control.Monad.IO.Class(liftIO)
import Data.List(partition)
import Messaging(IRC(..))
import Text.Regex.TDFA((=~))
import Types


botStating = do
  bs <- get
  io $ putStrLn $ nickname bs
  return ()

io :: IO a -> Bot a
io = liftIO

makeCommands mes = do
    bs <- get
    let always = [SayToTerm (show mes)]
    return $ always ++ case mes of
       (PING h)            -> [SayToServer Raw "" ("PONG :"++h), SayToTerm ("PONGED: "++h)]
       (PRIVMSG a b c d e) -> checkCannonRequest d $ concatMap (tryCommand bs mes) $ commands bs
       (JOIN a b c d)      -> [UserAdd d a]
       (PART a b c d)      -> [UserPart d a]
       (QUIT a b c d)      -> [UserQuit a]
       (SERV a b c d)      -> interpretServ mes
       (NICK a b c d)      -> [UserNick a d]
       _        -> []

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
resolveArg mes arg =
    case arg of
      NULL            -> []
      Literal s       -> s
      Nickname        -> nick mes
      Username        -> user mes
      FirstChannel    -> (mess mes) =~ "#[^ ]*" :: String
      Channel         -> chan mes
      Hostname        -> host mes
      WholeMessage    -> mess mes
      AllFields       -> show mes
      KarmaUP a       -> (++"++") $ resolveArg mes a
      KarmaDOWN a     -> (++"--") $ resolveArg mes a
      WordAfter r     -> let (_,_,a) = (mess mes) =~ r :: (String, String, String)
                             in head $ words a
      AllWordsAfter r -> let (_,_,a) = (mess mes) =~ r :: (String, String, String)
                             in drop 1 a
      SourceUrl       -> "https://github.com/MrRacoon/RaBot.git"

-- ------------------------------------------------------------------------------------------------------------------

makeAction :: Message -> C_Action -> BotAction
makeAction mes act =
    case act of
      Respond rt args dest    -> SayToServer rt (makeDestination mes dest) (unwords $ map (resolveArg mes) args)
      ReloadCommands          -> Reload (chan mes)
      LogToFile file args     -> Log (map (resolveArg mes) file) (concatMap (resolveArg mes) args)
      LoadCannons             -> CannonRequest
      CheckCannons            -> ShowPayload (chan mes)
      FireCannons             -> FirePayload
      HelpCommandList         -> DisplayHelp mes 1 False
      HelpCommandListAll      -> DisplayHelp mes 1 True
      HelpUsageList           -> DisplayHelp mes 2 False
      HelpUsageListAll        -> DisplayHelp mes 2 True
      HelpDescriptionList     -> DisplayHelp mes 3 False
      HelpDescriptionListAll  -> DisplayHelp mes 3 True
      ShowCurrentUsers        -> ShowUsers (chan mes)
      RunScript bin args dest -> Script bin (concatMap words $ map (resolveArg mes) args) (makeDestination mes dest)

loadable com =
    case com of
      SayToServer Privmsg _ _ -> True
      SayToTerm _             -> True
      _                       -> False

checkCannonRequest chan actions
      | CannonRequest `elem` actions    = let (load,rest) = partition loadable actions in (LoadPayload load) : rest 
      | otherwise                       = actions


makeDestination :: Message -> Destination -> String
makeDestination message dest =
    case dest of
      To_Current     -> chan message
      To_Server      -> []
      To_Channel s   -> s

-- ------------------------------------------------------------------------------------------------------------------

checkState :: Bool -> C_State -> Bool
checkState mst st =
    case (mst,st) of
      (_    , Never  ) -> False
      (_    , Always ) -> True
      (True , Active ) -> True
      (False, Passive) -> True
      _                -> False

-- ------------------------------------------------------------------------------------------------------------------

trig EmptyMessage [] = True
trig trigger mes =
    case trigger of
      AllMessages     -> True
      FirstWord x     -> (x==) $ head $ words mes
      WordPresent x   -> x `elem` words mes
      EntireMessage x -> x == mes
      FollowedBy x y  -> mes =~ (" ?"++x++" "++y++"( |$)") :: Bool
      _               -> False



