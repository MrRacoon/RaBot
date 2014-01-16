module Commanding where

-- ------------------------------------------------------------------------------------------------------------------
import Accessory
import Control.Monad.Trans.State(get,put)
import Control.Monad(void)
--import Control.Monad.IO.Class(liftIO)
import Data.List(intercalate)
import Messaging(IRC(..))
import Text.Regex.TDFA((=~))
import Types


-- ------------------------------------------------------------------------------------------------------------------
runCommands :: Bot ()
runCommands = do
    bs <- get
    let coms = commands bs
        m = currentMessage bs
        d = debug bs
        in do
          io $ debugIt d 1 ("Checking Message:" ++ show m)
          s <- preProcess
          case m of
             PING h            -> say Raw "" ("PONG :"++h)
             PRIVMSG {}        -> void (mapM (analyzeAndDo s) coms)
             JOIN n _ _ c      -> addUser c n
             PART n _ _ c      -> rmUser c n
             QUIT n _ _ _      -> delUser n
             SERV {}           -> interpretServerMessages
             _                 -> return ()

-- ------------------------------------------------------------------------------------------------------------------
interpretServerMessages :: Bot ()
interpretServerMessages = do
    bs <- get
    let m = currentMessage bs
        l = mess m
        c = code m
        in case c of
             "353" -> let (channel,_,names) = (l =~ " :"  :: (String,String,String))
                          in do mapM_ (addUser $ concat $ words channel) $ words names
                                return ()
             _     -> return ()

-- ------------------------------------------------------------------------------------------------------------------
--TODO
-- Ensure that the channel gets changed to the user's nick, if the channel is equal to the bot's nick
preProcess :: Bot Bool
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

-- ------------------------------------------------------------------------------------------------------------------
analyzeAndDo :: Bool -> Command -> Bot [()]
analyzeAndDo st command = do
    bs <- get
    let m          = currentMessage bs
        d          = debug bs
        authorized = checkAclList m $ ACL_M (Auth_Nick $ ownerNick bs) (Auth_User $ ownerUser bs) : auth command
        triggered  = all (trig $ mess m) (trigger command)
        stated     = checkState st (state command)
        in do
          io $ debugIt d 1 $ "\nChecking Command: " ++ show (name command)
          io $ debugIt d 2 $ "Message   : " ++ show m
          io $ debugIt d 2 $ "Authed    : " ++ show authorized
          io $ debugIt d 2 $ "Triggered : " ++ show triggered
          io $ debugIt d 2 $ "Stated    : " ++ show stated
          if authorized && triggered && stated
            then do
                io $ debugIt d 3 "Command Executing"
                mapM_ (io . debugIt d 3 . ("\tTriggered: "++) . show) $ action  command
                mapM performAction $ action command
            else do
                io $ debugIt d 3 "Command Discarded"
                return [()]

-- ------------------------------------------------------------------------------------------------------------------
performAction :: C_Action -> Bot ()
performAction act = do
    bs <- get
    --io $ putStrLn $ show bs
    let m = currentMessage bs
        logs = logsDirectory bs
    case act of
      KILL                    -> let channel = chan m
                                     in do
                                       say Notice channel "KILLSWITCH ENGAGED"
                                       say Quit channel "KILLSWITCH ENGAGED"
                                       error "KILLSWITCH ENGAGED"
      Respond rt args dest    -> do
                                 a <- resolveArg' args
                                 let destin   = makeDestination m dest
                                     in say rt destin $ unwords a
      ReloadCommands          -> reloadCommands
      LogToFile file args     -> do
                                 a <- resolveArg' file
                                 b <- resolveArg' args
                                 let  location = logs ++ "/" ++ intercalate "/" a   -- TODO CONCATMAP?
                                      line     = (++"\n") $ unwords b
                                      in io $ appendFile location line
      RunScript bin args dest -> do
                                 a <- resolveArg' args
                                 let arguments = concatMap words a
                                     destin    = makeDestination m dest
                                     binary    = scriptDirectory bs ++ "/" ++ bin
                                     in runScript binary arguments destin
      HelpCommandList         -> let coms = (unwords . filter (not . null) . map name . commands) bs
                                     dest = makeDestination m To_Current
                                     in say Privmsg dest coms
      HelpUsageList           -> let coms = (map usage . filter (not . null . name) . commands) bs
                                     dest = makeDestination m To_Current
                                     in void (mapM (say Privmsg dest) coms)
      HelpDescriptionList     -> let coms  = (filter (not . null . name) . commands) bs
                                     names = map name coms
                                     descs = map desc  coms
                                     outs  = normalize names descs
                                     dest  = makeDestination m To_Current
                                     in void (mapM (say Privmsg dest) outs)

-- ------------------------------------------------------------------------------------------------------------------
resolveArg' :: [Argument] -> Bot [String]
resolveArg' args = do
    bs <- get
    let m   = currentMessage bs
        f a = case a of
                NULL                  -> []
                SourceUrl             -> "https://github.com/MrRacoon/RaBot.git"
                Literal s             -> s
                WordAfter r           -> let (_,_,p) = mess m =~ r :: (String, String, String)
                                             in if null p then [] else head $ words p
                AllWordsAfter r       -> let (_,_,p) = mess m =~ r :: (String, String, String)
                                             in drop 1 p
                FirstChannelMentioned -> mess m =~ "#[^ ]*" :: String
                Message_Nickname      -> nick m
                Message_Username      -> user m
                Message_Hostname      -> host m
                Message_Channel       -> chan m
                Message_WholeMessage  -> mess m
                Message_AllFields     -> show m
                Bot_Nickname          -> nickname bs
                Bot_OwnerNick         -> ownerNick bs
                Bot_OwnerUser         -> ownerUser bs
                Bot_AttChar           -> attentionCharacter bs
                Bot_Server            -> server bs
                Bot_Port              -> port bs
                Bot_Channels          -> unwords $ map fst $ channels bs
                Bot_CommandCount      -> show $ length $ commands bs
                Bot_CommandDirectory  -> commandDirectory bs
                Bot_ScriptDirectory   -> scriptDirectory bs
                Bot_LogDirectory      -> logsDirectory bs
                Bot_PayloadCount      -> show $ length $ payload bs
                Bot_PayloadLoaded     -> show $ not $ null $ payload bs
                Bot_DebugLevel        -> show $ debug bs
                _                     -> []
              in return $ map f args

-- ------------------------------------------------------------------------------------------------------------------
makeDestination :: Message -> Destination -> String
makeDestination message dest =
    case dest of
      To_Current     -> chan message
      To_Server      -> []
      To_Channel s   -> s

-- ------------------------------------------------------------------------------------------------------------------
checkAclList :: Message -> [ACL] -> Bool
checkAclList  message = any (checkAcl message)
checkAcl :: Message -> ACL -> Bool
checkAcl message acl =
            case acl of
               ACL_N         -> True
               (ACL_W a)     -> authed message a
               (ACL_M a b)   -> all (authed message) [a,b]
               (ACL_S a b c) -> all (authed message) [a,b,c]
               where
                 authed message' (Auth_Nick n) = n == nick message'
                 authed message' (Auth_User u) = u == user message'
                 authed message' (Auth_Host h) = h == host message'

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
trig :: String -> C_Trigger -> Bool
trig []  EmptyMessage = True
trig mes trigger' =
    case trigger' of
      AllMessages     -> True
      FirstWord x     -> not  (null mes) && ((x==) $ head $ words mes)
      WordPresent x   -> x `elem` words mes
      EntireMessage x -> x == mes
      FollowedBy x y  -> mes =~ (" ?"++x++" "++y++"( |$)") :: Bool
      _               -> False
