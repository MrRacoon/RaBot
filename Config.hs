module Config where


data BotConfig = BotConfig { bot_nickname    :: String
                           , bot_Char        :: String
                           , bot_ownerNick   :: String
                           , bot_ownerUser   :: String
                           , bot_server      :: String
                           , bot_port        :: String
                           , bot_chans       :: [String]
                           , bot_commandDir  :: String
                           , bot_logDir      :: String
                           , bot_scriptDir   :: String
                           , bot_debugLevel  :: Int
                           }

resolveArguments config []                     = config
resolveArguments config ("-n":arg:xs)          = resolveArguments (config { bot_nickname    = arg }) xs
resolveArguments config ("--nick":arg:xs)      = resolveArguments (config { bot_nickname    = arg }) xs
resolveArguments config ("-a":arg:xs)          = resolveArguments (config { bot_Char        = arg }) xs
resolveArguments config ("--att":arg:xs)       = resolveArguments (config { bot_Char        = arg }) xs
resolveArguments config ("-s":arg:xs)          = resolveArguments (config { bot_server      = arg }) xs
resolveArguments config ("--server":arg:xs)    = resolveArguments (config { bot_server      = arg }) xs
resolveArguments config ("-p":arg:xs)          = resolveArguments (config { bot_port        = arg }) xs
resolveArguments config ("--port":arg:xs)      = resolveArguments (config { bot_port        = arg }) xs
resolveArguments config ("-c":arg:xs)          = resolveArguments (config { bot_chans       = bot_chans config ++ [arg] }) xs
resolveArguments config ("--channel":arg:xs)   = resolveArguments (config { bot_chans       = bot_chans config ++ [arg] }) xs
resolveArguments config ("-C":arg:xs)          = resolveArguments (config { bot_commandDir  = arg }) xs
resolveArguments config ("--command":arg:xs)   = resolveArguments (config { bot_commandDir  = arg }) xs
resolveArguments config ("-L":arg:xs)          = resolveArguments (config { bot_logDir      = arg }) xs
resolveArguments config ("--log":arg:xs)       = resolveArguments (config { bot_logDir      = arg }) xs
resolveArguments config ("-S":arg:xs)          = resolveArguments (config { bot_scriptDir   = arg }) xs
resolveArguments config ("--script":arg:xs)    = resolveArguments (config { bot_scriptDir   = arg }) xs
resolveArguments config ("-N":arg:xs)          = resolveArguments (config { bot_ownerNick   = arg }) xs
resolveArguments config ("--ownerNick":arg:xs) = resolveArguments (config { bot_ownerNick   = arg }) xs
resolveArguments config ("-U":arg:xs)          = resolveArguments (config { bot_ownerUser   = arg }) xs
resolveArguments config ("--ownerUser":arg:xs) = resolveArguments (config { bot_ownerUser   = arg }) xs
resolveArguments config ("-h":_)               = error programUsage
resolveArguments config ("--help":_)           = error programUsage
resolveArguments _       (x:xs)                = error ("Error: Incorrect Argument: "++x++"\n"++programUsage)


programUsage = "usage: rabot [Configuration Parameters]\n\n"
      ++"\t-n Nickname\n\n"
      ++"\t-a attentionChar\n\n"
      ++"\t-s Server\n\n"
      ++"\t-p port\n\n"
      ++"\t-c channel\n\n"
      ++"\t-C commandDirectory\n\n"
      ++"\t-L logDirectory\n\n"
      ++"\t-S scriptDirectory\n\n"
      ++"\t-N ownerNickname\n\n"
      ++"\t-U ownerUsername\n\n"
      ++"\t-h help\n\n"


