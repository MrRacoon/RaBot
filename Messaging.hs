module Messaging where

import Secrets
import Text.Regex.TDFA

-- ------------------------------------------------------------------
-- Types
--
type Nick = String
type User = String
type Host = String
type Mess = String
type Chan = String
type Code = String

-- ------------------------------------------------------------------
-- Message
-- Every IRC Message will be parsed in at least one of the following.
--
data Message = PRIVMSG Nick User Host Chan Mess
             | JOIN Nick User Host Chan
             | PART Nick User Host Chan
             | QUIT Nick User Host Mess
             | NICK Nick User Host Nick
             | PING Host
             | SERV Host Code Nick Mess
             | UNKNOWN String
    deriving (Show,Read,Eq)

-- ------------------------------------------------------------------
-- IRC Class
-- Functions that can be applied to any of the Message types.
class IRC a where
    nick :: a -> Nick
    user :: a -> User
    host :: a -> Host
    chan :: a -> Chan
    mess :: a -> Mess
    code :: a -> Code
    natv :: a -> String
    reed :: String -> [(a,String)]

-- ------------------------------------------------------------------
-- IRC Instances
--
instance IRC Message where
    nick (PRIVMSG n _ _ _ _)   =  n
    nick (JOIN n _ _ _)        =  n
    nick (PART n _ _ _)        =  n
    nick (QUIT n _ _ _)        =  n
    nick (NICK n _ _ _)        =  n
    nick _                     = []
    user (PRIVMSG _ u _ _ _)   =  u
    user (JOIN _ u _ _)        =  u
    user (PART _ u _ _)        =  u
    user (QUIT _ u _ _)        =  u
    user (NICK _ u _ _)        =  u
    user _                     = []
    host (PRIVMSG _ _ h _ _)   =  h
    host (JOIN _ _ h _)        =  h
    host (PART _ _ h _)        =  h
    host (QUIT _ _ h _)        =  h
    host (NICK _ _ h _)        =  h
    host _                     = []
    chan (PRIVMSG _ _ _ c _)   =  c
    chan (JOIN _ _ _ c)        =  c
    chan (PART _ _ _ c)        =  c
    chan _                     = []
    mess (PRIVMSG _ _ _ _ m)   =  m
    mess (QUIT _ _ _ m)        =  m
    mess (NICK _ _ _ m)        =  m
    mess (SERV _ _ _ m)        = []
    mess _                     = []
    code (SERV _ c _ _)        =  c
    code _                     = []
    natv (PRIVMSG n u h c m)   = ":"++n++"!"++u++"@"++h++" PRIVMSG "++c++" :"++m
    natv (JOIN n u h c)        = ":"++n++"!"++u++"@"++h++" JOIN "++c
    natv (PART n u h c)        = ":"++n++"!"++u++"@"++h++" PART "++c
    natv (QUIT n u h m)        = ":"++n++"!"++u++"@"++h++" QUIT "++" :"++m
    natv (NICK n u h m)        = ":"++n++"!"++u++"@"++h++" NICK "++" :"++m
    natv (SERV h c n m)        = ":"++h++": "++c++"@"++n++" QUIT "++" :"++m
    natv (PING h)              = ":"++h++" :PING"
    natv (UNKNOWN s)           = s
    reed s                     = readMessage s

-- ------------------------------------------------------------------
-- Parse
-- Takes a Message as a String and converts it into a Message Type
--
parse :: String -> Message
parse str = case readMessage str of
              (x:_) -> fst x
              _     -> UNKNOWN str

-- ------------------------------------------------------------------
-- readMessage
-- Core Message Conversion Method
--
readMessage :: ReadS Message
readMessage s =  [ (PING hst, []) 
                 | ("PING",hst) <- s `till` ':' ]
              ++ [ (PRIVMSG nic usr hst chn (tail es), []) 
                 | (":",zs) <- lex s
                 , (nic,as) <- zs `till` '!'
                 , (usr,bs) <- as `till` '@'
                 , (hst,cs) <- bs `till` ' '
                 , ("PRIVMSG",ds) <- cs `till` ' '
                 , (chn,es) <- ds `till` ' ' ]
              ++ [ (JOIN nic usr hst chn, []) | (":",zs) <- lex s
                 , (nic,as) <- zs `till` '!'
                 , (usr,bs) <- as `till` '@'
                 , (hst,cs) <- bs `till` ' '
                 , ("JOIN",chn) <- cs `till` ' ' ]
              ++ [ (PART nic usr hst chn, []) | (":",zs) <- lex s
                 , (nic,as) <- zs `till` '!'
                 , (usr,bs) <- as `till` '@'
                 , (hst,cs) <- bs `till` ' '
                 , ("PART",chn) <- cs `till` ' ' ]
              ++ [ (QUIT nic usr hst mes, [])
                 | (":",zs) <- lex s
                 , (nic,as) <- zs `till` '!'
                 , (usr,bs) <- as `till` '@'
                 , (hst,cs) <- bs `till` ' '
                 , ("QUIT",mes) <- cs `till` ' ']
              ++ [ (NICK nic usr hst mes, [])
                 | (":",zs) <- lex s
                 , (nic,as) <- zs `till` '!'
                 , (usr,bs) <- as `till` '@'
                 , (hst,cs) <- bs `till` ' '
                 , ("NICK",mes) <- cs `till` ' ' ]
              ++ [ (SERV hst cde nic mes, [])
                 | (":",zs) <- lex s
                 , (hst,as) <- zs `till` ' '
                 , (cde,bs) <- as `till` ' '
                 , (nic,mes) <- bs `till` ' ' 
                 , nic == botNick
                 , (cde =~ "[0-9]{3}" :: Bool) ]

-- ------------------------------------------------------------------
-- Accesory Funtions
--
till :: Eq a => [a] -> a -> [([a], [a])]
till [] _         = []
till xs c         = till' c [] xs
till' c as []     = []
till' c as (b:bs)
    | b == c      = [(reverse as,bs)]
    | otherwise   = till' c (b:as) bs

isPRIVMSG s = (s =~ "^:[^!]+![^@]+@[^ ]+ PRIVMSG [#]?[^ ]* :.*$"   :: Bool)
isJOIN    s = (s =~ "^:[^!]+![^@]+@[^ ]+ JOIN [#]?[^ ]*$"          :: Bool)
isPART    s = (s =~ "^:[^!]+![^@]+@[^ ]+ PART [#]?[^ ]*$"          :: Bool)
isQUIT    s = (s =~ "^:[^!]+![^@]+@[^ ]+ QUIT [#]?[^ ]*$ :.*"      :: Bool)
isNICK    s = (s =~ "^:[^!]+![^@]+@[^ ]+ NICK :[^ ]+$"             :: Bool)
isSERV    s = (s =~ ("^:[^:]+: [0-9]{3} "++botNick++" [:]?[^ ]*$") :: Bool)
isPING    s = (s =~ "^:[^:]+ :PING" :: Bool)


