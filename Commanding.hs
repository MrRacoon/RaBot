module Commanding where

import Messaging
import Text.Regex.TDFA

data Command = Command { state   :: C_State      -- state that the command is in
                       , auth    :: [String]     -- People with authority to issue the command
                       , usage   :: String       -- How to use the command
                       , desc    :: String       -- Description of the command
                       , trigger :: [C_Trigger]  -- How to trigger the command
                       , action  :: [C_Action] } -- What the command will do
    deriving Show

data C_State = Active   -- Active state involves the issuer talking directly to the bot
             | Passive  -- Passive state is in affect in all cases where the bot is not directly being instigated
             | Always   -- bot will always attempt to run the command regardless of context
             | Never    -- The bot will never in a million years ever issue the command, it may as well not even exist
    deriving Show

data C_Trigger = FirstWord String          -- The first word is <String>
               | WordPresent String        -- The <String> is present
    deriving Show

data C_Action = Respond [Argument] Destination -- Respond with the string at the channel specified by destination
    deriving Show

type Regex_Text = String
data Argument = Literal String      -- Stands for the literal string
              | WordAfter Regex_Text     -- return the first word after some regex
              | AllWordsAfter Regex_Text -- Return everything after the Regex
    deriving Show

data Destination = To_Current        -- The destination is to the current window in which some essage was recieved
                 | To_Server         -- The destination is no where in particular aside from the server itself RAW
    deriving Show

commandExample1 = Command { state   = Active
                          , auth    = ["Racoon"]
                          , usage   = "say 'Pst'"
                          , desc    = "Test that the bot is actually working"
                          , trigger = [WordPresent "Pst"]
                          , action  = [Respond [Literal "Hey I heard That"] To_Current ] }

data BotAction = SayToServer String String
    deriving Show

tryCommand :: Message -> Command -> Maybe [BotAction]
tryCommand message command
    | authorized && triggered = Just $ map (makeAction message) (action command)
    | otherwise               = Nothing
  where
    authorized = (nick message) `elem` (auth command)
    triggered  = all (flip trig (mess message)) (trigger command)


trig :: C_Trigger -> (String -> Bool)
trig (FirstWord w) = (==w) . head . words
trig (WordPresent w) = (elem w) . words

makeAction message (Respond args dest) = SayToServer (makeDestination message dest) (unwords $ map (resolveArgs message) args)

makeDestination message To_Current = chan message
makeDestination _       To_Server  = []

resolveArgs message (Literal s)       = s
resolveArgs message (WordAfter r)     = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in head $ words a
resolveArgs message (AllWordsAfter r) = let (_,_,a) = (mess message) =~ r :: (String, String, String)
                                        in a
