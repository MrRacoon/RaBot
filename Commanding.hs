module Commanding where



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
               | FollowedBy String String  -- the first <String> is followed by the second <String>
    deriving Show


data C_Action = Respond String Destination
    deriving Show

data Destination = To_Current
                 | To_Channel String
                 | To_Nick String
                 | To_Server
    deriving Show

commandExample1 = Command { state   = Active
                          , auth    = ["Racoon"]
                          , usage   = "say 'Pst'"
                          , desc    = "Test that the bot is actually working"
                          , trigger = [WordPresent "Pst"]
                          , action  = [Respond "Hey I heard That" To_Current ] }




