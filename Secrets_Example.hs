module Secrets where

-- the nickname of the bot's owner, this gives absolute priviledge
-- for all of the user defined commands
owner       = "OwnersNick"

-- Server the bot will connect to
botServer   = "chat.freenode.org"

-- Three is no SSL yet, therefore you shouldn't use any ports
-- that are going to expect it
botPort     = 6667

-- the initial channel the bot will connect to
initChan    = "#SomeChannel"

-- Nickname of the bot, also the attChar, both of which activate
-- active commands
botNick     = "BotName"
attChar     = ">>="

-- Description of the bot
botDesc     = "My Haskell Bot"

-- Depricated at this point, but still currently needed in the bot
-- to startup
initMasters = [owner]

-- Filename in which the commands will be loaded
commandFile = "Commands"

-- Where are the logs going to be storred
logFolder   = "./logs/"
