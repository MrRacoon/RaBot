module Triggers where


-- ------------------------------------------------------------------------------------------------------------------
-- Triggers
-- How will we know when to execute a given command? One Word: Triggers
--    AllMessages -> Triggers command for all messages
--    FirstWord   -> The first word in the message matches a given regex
--    WordPresent -> Some string matching the Regex is present in the message
--
data C_Trigger = AllMessages
               | FirstWord String
               | WordPresent String
               | EntireMessage String
    deriving (Show,Read,Eq)

trig :: C_Trigger -> (String -> Bool)
trig AllMessages      = const True
trig (FirstWord w)    = (==w) . head . words
trig (WordPresent w)  = (elem w) . words
trig (EntireMessage r) = (r==)

