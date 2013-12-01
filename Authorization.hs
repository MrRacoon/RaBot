module Authorization where


import Messaging
import Secrets

-- ------------------------------------------------------------------------------------------------------------------
-- Authorization
-- Specify who can execute Commands, all auth tokens specify a user, or group that has sufficient access to execute
-- the command it is included in. A null list stands for anyone.
--    Auth_Nick -> Nickname matches sender
--    Auth_User -> Username matches sender
--    Auth_Host -> Hostname matches sender
--
data ACL = ACL_N
         | ACL_W Authorization
         | ACL_M Authorization Authorization
         | ACL_S Authorization Authorization Authorization
    deriving (Show,Read,Eq)

checkAclList  message []        = checkAcl message (ACL_M (Auth_Nick botOwnerNick) (Auth_User botOwnerUser))
checkAclList  message list      = checkAclList' message list
checkAclList' message (x:xs)    = (checkAcl message x) || (checkAclList' message xs)

checkAcl message ACL_N         = True
checkAcl message (ACL_W a)     = (authed message a)
checkAcl message (ACL_M a b)   = (authed message a) && (authed message b)
checkAcl message (ACL_S a b c) = (authed message a) && (authed message b) && (authed message c)

data Authorization = Auth_Nick String
                   | Auth_User String
                   | Auth_Host String
    deriving (Show,Read,Eq)

authed message (Auth_Nick n) = n == (nick message)
authed message (Auth_User u) = u == (user message)
authed message (Auth_Host h) = h == (host message)

