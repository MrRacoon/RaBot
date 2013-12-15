module Authorization where


import Messaging
import Types


checkAclList  message []        = False
checkAclList  message list      = checkAclList' message list
checkAclList' message (x:xs)    = (checkAcl message x) || (checkAclList' message xs)

checkAcl message ACL_N         = True
checkAcl message (ACL_W a)     = (authed message a)
checkAcl message (ACL_M a b)   = (authed message a) && (authed message b)
checkAcl message (ACL_S a b c) = (authed message a) && (authed message b) && (authed message c)


authed message (Auth_Nick n) = n == (nick message)
authed message (Auth_User u) = u == (user message)
authed message (Auth_Host h) = h == (host message)

