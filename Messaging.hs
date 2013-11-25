module Messaging where

import Secrets
import Test.QuickCheck
-- ------------------------------------------------------------------
-- Types
--
type Nick = String
type User = String
type Host = String
type Chan = String
type Mess = String
type Code = String

data NICKNAME = NICKNAME
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
    reed :: ReadS a

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
    natv (PRIVMSG n u h c m)   = ":"++n++"!"++u++"@"++h++" PRIVMSG "++c++" :"++m++"\r"
    natv (JOIN n u h c)        = ":"++n++"!"++u++"@"++h++" JOIN "++c++"\r"
    natv (PART n u h c)        = ":"++n++"!"++u++"@"++h++" PART "++c++"\r"
    natv (QUIT n u h m)        = ":"++n++"!"++u++"@"++h++" QUIT "++":"++m++"\r"
    natv (NICK n u h m)        = ":"++n++"!"++u++"@"++h++" NICK "++":"++m++"\r"
    natv (SERV h c n m)        = ":"++h++" "++c++" "++n++" :"++m++"\r"
    natv (PING h)              = "PING :"++h++"\r"
    natv (UNKNOWN s)           = s++"\r"
    reed                       = readMessage

-- ------------------------------------------------------------------
-- Parse
-- Takes a Message as a String and converts it into a Message Type
--
parse :: String -> Message
parse str = case readMessage str of
              []    -> UNKNOWN str
              (x:_) -> fst x

parseList :: String -> [Message]
parseList []  = []
parseList str = case readMessage str of
              ((x,[]):res)  -> x : []
              ((x,xs):res)  -> x : parseList xs
              _             -> let [(a,as)] = str `till` '\n' in (UNKNOWN a) : (parseList as)

toSerial :: [Message] -> String
toSerial = concatMap natv

-- ------------------------------------------------------------------
-- readMessage
-- Core Message Conversion Method
--
readMessage :: ReadS Message
readMessage s =  [ (PING hst, res)
                 | ("PING",as)    <- s `till` ' '
                 , (hst,res)      <- (tail as) `till` '\r']
              ++ [ (SERV hst cde nic mes, res)
                 | (":",zs)       <- [splitAt 1 s]
                 , (hst,as)       <- zs `till` ' '
                 , (cde,bs)       <- as `till` ' '
                 , (nic,cs)       <- bs `till` ' '
                 , (mes,res)      <- (tail cs) `till` '\r'
                 , nic == botNick
                 , (length cde) == 3 ]
              ++ [ (PRIVMSG nic usr hst chn mes, res)
                 | (":",zs)       <- [splitAt 1 s]
                 , (nic,as)       <- zs `till` '!'
                 , (usr,bs)       <- as `till` '@'
                 , (hst,cs)       <- bs `till` ' '
                 , ("PRIVMSG",ds) <- cs `till` ' '
                 , (chn,es)       <- ds `till` ' '
                 , (mes,res)      <- (tail es) `till` '\r'
                 , (length nic) <= 18 ]
              ++ [ (JOIN nic usr hst chn, res)
                 | (":",zs)       <- [splitAt 1 s]
                 , (nic,as)       <- zs `till` '!'
                 , (usr,bs)       <- as `till` '@'
                 , (hst,cs)       <- bs `till` ' '
                 , ("JOIN",ds)    <- cs `till` ' '
                 , (chn,res)      <- ds `till` '\r'
                 , (length nic) <= 18 ]
              ++ [ (PART nic usr hst chn, res)
                 | (":",zs)       <- [splitAt 1 s]
                 , (nic,as)       <- zs `till` '!'
                 , (usr,bs)       <- as `till` '@'
                 , (hst,cs)       <- bs `till` ' '
                 , ("PART",ds)    <- cs `till` ' '
                 , (chn,res)      <- ds `till` '\r'
                 , (length nic) <= 18 ]
              ++ [ (QUIT nic usr hst mes, res)
                 | (":",zs)       <- [splitAt 1 s]
                 , (nic,as)       <- zs `till` '!'
                 , (usr,bs)       <- as `till` '@'
                 , (hst,cs)       <- bs `till` ' '
                 , ("QUIT",ds)    <- cs `till` ' '
                 , (mes,res)      <- (tail ds) `till` '\r'
                 , (length nic) <= 18 ]
              ++ [ (NICK nic usr hst mes, res)
                 | (":",zs)       <- [splitAt 1 s]
                 , (nic,as)       <- zs `till` '!'
                 , (usr,bs)       <- as `till` '@'
                 , (hst,cs)       <- bs `till` ' '
                 , ("NICK",ds)    <- cs `till` ' '
                 , (mes,res)      <- (tail ds) `till` '\r'
                 , (length nic) <= 18 ]

till :: Eq a => [a] -> a -> [([a], [a])]
till [] _         = []
till xs c         = till' c [] xs
till' c as []     = []
till' c as (b:bs)
    | b == c      = [(reverse as,bs)]
    | otherwise   = till' c (b:as) bs

-- ------------------------------------------------------------------
-- Testing Suite
--

testMessaging' n = testParser' n >> testSerialization' n

testParser    = testParser' 1000
testParser' n = quickCheckWith stdArgs {maxSuccess = n} ((\x -> (parse $ natv x) == x) :: (Message -> Bool))

testSerialization    = testSerialization' 1000
testSerialization' n = quickCheckWith stdArgs {maxSuccess = n} ((\x -> (parseList $ toSerial x) == x) :: ([Message] -> Bool))

instance Arbitrary Message where
    arbitrary = do
      n <- choose (0,6) :: Gen Int
      a <- randomNick
      b <- randomUser
      c <- randomHost
      d <- randomChannel
      e <- randomMessage
      f <- randomCode
      g <- randomNick
      h <- randomUnknown
      return $ case n of
        0 -> PRIVMSG a b c d e
        1 -> JOIN a b c d
        2 -> PART a b c d
        3 -> QUIT a b c e
        4 -> NICK a b c g
        5 -> PING c
        6 -> SERV c f botNick h

letters = (['a'..'z']++['A'..'Z']) :: [Char]
numbers :: [Char]
numbers = ['0'..'9']

randomStringOf = (listOf1 . elements)

randomNick     = let possibleCharacters = (letters++numbers++['_','^','-'])
                 in (randomStringOf possibleCharacters) >>= (return . take 18)

randomUser     = let possibleFirstLetters = (letters++['_'])
                     possibleCharacters   = (letters++numbers++['_'])
                 in (elements possibleFirstLetters) >>= (\a -> (randomStringOf possibleCharacters >>= (\b -> return (a:b))))

randomHost     = let possibleCharacters = (letters++numbers++['-'])
                 in do
                    a <- randomStringOf possibleCharacters
                    b <- randomStringOf possibleCharacters
                    c <- randomStringOf possibleCharacters
                    d <- randomStringOf possibleCharacters
                    return (a++"."++b++"."++c++"."++d)

randomChannel  = let possibleCharacters = (letters++numbers)
                 in (listOf $ elements possibleCharacters) >>= (return . ('#':))

randomCode     = let possibleCharacters = numbers
                 in do
                   a <- elements possibleCharacters
                   b <- elements possibleCharacters
                   c <- elements possibleCharacters
                   return [a,b,c]

randomMessage  = let possibleCharacters = [' '..'~']
                 in randomStringOf possibleCharacters >>= (return)

randomUnknown  = let possibleCharacters = ['\NUL'..]
                 in (randomStringOf possibleCharacters) >>= return

randomSerial   = (arbitrary :: Gen [Message]) >>= return
