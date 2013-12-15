module Actions where

import Arguments
import Data.List(partition)
import Messaging
import Types



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

