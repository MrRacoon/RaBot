module Triggers where

import Text.Regex.TDFA
import Types

trig :: C_Trigger -> String -> Bool
trig EmptyMessage      [] = True
trig _                 [] = False
trig AllMessages       m  = True
trig (FirstWord w)     m  = (w==) $ head $ words m
trig (WordPresent w)   m  = elem w $ words m
trig (EntireMessage r) m  = r == m
trig (FollowedBy a b)  m  = m =~ (" ?"++a++" "++b++"( |$)") :: Bool
trig _                 _  = False
