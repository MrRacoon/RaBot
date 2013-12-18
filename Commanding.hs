module Commanding where

-- ------------------------------------------------------------------------------------------------------------------

import Accessory
import Control.Monad.Trans.State(get,put)
import Control.Monad.IO.Class(liftIO)
import Data.List(partition, intersperse)
import Messaging(IRC(..))
import Text.Regex.TDFA((=~))
import Text.Printf
import Types


--makeCommands mes = do
--    bs <- get
--    let always = [SayToTerm (show mes)]
--    return $ always ++ case mes of
--       (PING h)            -> [SayToServer Raw "" ("PONG :"++h), SayToTerm ("PONGED: "++h)]
--       (PRIVMSG a b c d e) -> checkCannonRequest d $ concatMap (tryCommand bs mes) $ commands bs
--       (JOIN a b c d)      -> [UserAdd d a]
--       (PART a b c d)      -> [UserPart d a]
--       (QUIT a b c d)      -> [UserQuit a]
--       (SERV a b c d)      -> interpretServ mes
--       (NICK a b c d)      -> [UserNick a d]
--       _        -> []
--
----parseCommands :: Message -> [Command] -> Maybe [BotAction]
--parseCommands :: BotState -> Message -> [Command] -> [BotAction]
--parseCommands botState mes com
--     = let always  = [SayToTerm (show mes)]
--       in (always++) $ case mes of
--          (PING h)            -> [SayToServer Raw "" ("PONG :"++h), SayToTerm ("PONGED: "++h)]
--          (PRIVMSG a b c d e) -> checkCannonRequest d $ concatMap (tryCommand botState mes) com
--          (JOIN a b c d)      -> [UserAdd d a]
--          (PART a b c d)      -> [UserPart d a]
--          (QUIT a b c d)      -> [UserQuit a]
--          (SERV a b c d)      -> interpretServ mes
--          (NICK a b c d)      -> [UserNick a d]
--          _        -> []
--
--
--
--interpretServ (SERV a "353" c d) = let (chan:nics) = words d
--                                       list        = words $ tail $ unwords nics
--                                   in map (UserAdd chan) list
--interpretServ _ = []

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

--tryCommand :: BotState -> Message -> Command -> [BotAction]
--tryCommand botState m@(PRIVMSG nic usr hst chn mes) command
--    | copesetic = map (makeAction (PRIVMSG nic usr hst chn messg)) (action command)
--    | otherwise = []
--  where
--    copesetic  = authorized && triggered && stated
--    triggered  = all (trig $ mes) (trigger command)
--    authorized = checkAclList m $ (ACL_M (Auth_Nick $ ownerNick botState) (Auth_User $ ownerUser botState)) : auth command
--    (a,b,c)    = (mes =~ ("(^"++(nickname botState)++"[:]? |^"++(attentionCharacter botState)++" )") :: (String, String, String))
--    st         = not $ null b
--    messg      = if st then c else a
--    stated     = checkState st (state command)

runCommands = do
    bs <- get
    let c = commands bs
        m = currentMessage bs
        in do
          s <- preProcess
          case m of
             PING h            -> say Raw "" ("PONG :"++h)
             PRIVMSG _ _ _ _ _ -> mapM (analyzeAndDo s) c >> return ()
             _                 -> return ()


preProcess = do
    bs <- get
    let m       = currentMessage bs
        n       = nickname bs
        att     = attentionCharacter bs
        (a,b,c) = (mess m =~ ("(^"++n++"[:]? |^"++att++" )") :: (String, String, String))
        st      = not $ null b
        messg   = if st then c else a
        in do
          put bs { currentMessage = putMessage messg m }
          return st


analyzeAndDo st command = do
    bs <- get
    let m          = currentMessage bs
        d          = debug bs
        authorized = checkAclList m $ (ACL_M (Auth_Nick $ ownerNick bs) (Auth_User $ ownerUser bs)) : auth command
        triggered  = all (trig $ mess m) (trigger command)
        stated     = checkState st (state command)
        in do
          io $ debugIt d 1 $ "\nChecking Command: "++(show $ name command)
          io $ debugIt d 2 $ "Message   : "++ (show $ m)
          io $ debugIt d 2 $ "Authed    : "++ (show authorized)
          io $ debugIt d 2 $ "Triggered : "++ (show triggered)
          io $ debugIt d 2 $ "Stated    : "++ (show stated)
          if authorized && triggered && stated
            then do
                io $ debugIt d 3 "Command Executing"
                mapM (io . debugIt d 3 . ("\tTriggered: "++) . show) $ action  command
                mapM performAction $ action command
            else do
                io $ debugIt d 3 "Command Discarded"
                return [()]

performAction act = do
    bs <- get
    --io $ putStrLn $ show bs
    let m = currentMessage bs
        h = handle bs
        logs = logsDirectory bs
    case act of
      KILL                    -> let channel = chan m
                                     in do
                                       say Notice channel "KILLSWITCH ENGAGED"
                                       error "KILLSWITCH ENGAGED"
      Respond rt args dest    -> let response = (unwords . map (resolveArg m)) args
                                     destin   = makeDestination m dest
                                     in say rt destin response
      ReloadCommands          -> let destin = chan m
                                     in do
                                       (ers,sucs) <- io $ loadCommandDir $ commandDirectory bs
                                       mapM (\(f,c) -> say Notice destin $ ("Loaded: "++f)) sucs
                                       mapM (\(f,e) -> say Notice destin e) ers
                                       put $ bs { commands = (concatMap snd sucs) }
      LogToFile file args     -> let location = logs ++ "/" ++ (concat $ intersperse "/" $ map (resolveArg m) file)   -- TODO CONCATMAP?
                                     line     = (++"\n") $ concatMap (resolveArg m) args
                                     in do
--                                       io $ putStrLn (line ++ "->\n\t" ++ location)
                                       io $ appendFile location line
      RunScript bin args dest -> let arguments = concatMap words $ map (resolveArg m) args
                                     destin    = makeDestination m dest
                                     binary    = scriptDirectory bs ++ "/" ++ bin
                                     in runScript binary arguments destin

--      RunScript bin args dest -> Script bin (concatMap words $ map (resolveArg mes) args) (makeDestination mes dest)
--      ShowCurrentUsers        -> ShowUsers (chan mes)
--      HelpCommandList         -> DisplayHelp mes 1 False
--      HelpCommandListAll      -> DisplayHelp mes 1 True
--      HelpUsageList           -> DisplayHelp mes 2 False
--      HelpUsageListAll        -> DisplayHelp mes 2 True
--      HelpDescriptionList     -> DisplayHelp mes 3 False
--      HelpDescriptionListAll  -> DisplayHelp mes 3 True
--      LoadCannons             -> CannonRequest
--      CheckCannons            -> ShowPayload (chan mes)
--      FireCannons             -> FirePayload

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

trig []  EmptyMessage = True
trig mes trigger =
    case trigger of
      AllMessages     -> True
      FirstWord x     -> (x==) $ head $ words mes
      WordPresent x   -> x `elem` words mes
      EntireMessage x -> x == mes
      FollowedBy x y  -> mes =~ (" ?"++x++" "++y++"( |$)") :: Bool
      _               -> False
