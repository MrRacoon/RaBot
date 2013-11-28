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
               | EmptyMessage
    deriving (Show,Read,Eq)

trig :: C_Trigger -> String -> Bool
trig EmptyMessage      [] = True
trig _                 [] = False
trig AllMessages       m  = True
trig (FirstWord w)     m  = (w==) $ head $ words m
trig (WordPresent w)   m  = elem w $ words m
trig (EntireMessage r) m  = r == m
trig _                 _  = False
