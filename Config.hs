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
                           } deriving (Show,Read)

makeConfig :: IO BotConfig
makeConfig = do
  putStrLn "What would you like the name of the Bot to be?:"
  nick <- getLine
  putStrLn "What character(s) would you like the bot to respond to, along with its name?:"
  atc <- getLine
  putStrLn "What is your nickname on IRC?:"
  onick <- getLine
  putStrLn "What is the username you use when connecting to IRC?:"
  ouser <- getLine
  putStrLn "What is the IRC server address?:"
  server <- getLine
  putStrLn "What port would you like to connect to?:"
  port <- getLine
  putStrLn "Which channels would you like the bot to join initially?:"
  chans <- getLine
  putStrLn "What Directory would you like the bot to load the command files from?:"
  cdir <- getLine
  putStrLn "What directory would you like the bot to put logfiles?:"
  ldir <- getLine
  putStrLn "What directory do you keep scripts that you would like the bot to be able to run?:"
  sdir <- getLine
  putStrLn "Between 1-10, how much debugging output would you like to see?:"
  debug <- getLine
  let config = BotConfig nick atc onick ouser server port (words chans) cdir ldir sdir (read debug)
  writeFile "InitialConfig" $ show config
  return config

resolveArguments :: BotConfig -> [String] -> BotConfig
resolveArguments config []                        = config
resolveArguments config ("-n":arg:xs)             = resolveArguments (config { bot_nickname    = arg }) xs
resolveArguments config ("--nick":arg:xs)         = resolveArguments (config { bot_nickname    = arg }) xs
resolveArguments config ("-pn":arg:xs)            = resolveArguments (config { bot_nickname    = arg++bot_nickname config }) xs
resolveArguments config ("--prepend-nick":arg:xs) = resolveArguments (config { bot_nickname    = arg++bot_nickname config }) xs
resolveArguments config ("-an":arg:xs)            = resolveArguments (config { bot_nickname    = bot_nickname config++arg }) xs
resolveArguments config ("--append-nick":arg:xs)  = resolveArguments (config { bot_nickname    = bot_nickname config++arg }) xs
resolveArguments config ("-a":arg:xs)             = resolveArguments (config { bot_Char        = arg }) xs
resolveArguments config ("--att":arg:xs)          = resolveArguments (config { bot_Char        = arg }) xs
resolveArguments config ("-s":arg:xs)             = resolveArguments (config { bot_server      = arg }) xs
resolveArguments config ("--server":arg:xs)       = resolveArguments (config { bot_server      = arg }) xs
resolveArguments config ("-p":arg:xs)             = resolveArguments (config { bot_port        = arg }) xs
resolveArguments config ("--port":arg:xs)         = resolveArguments (config { bot_port        = arg }) xs
resolveArguments config ("-c":arg:xs)             = resolveArguments (config { bot_chans       = bot_chans config ++ [arg] }) xs
resolveArguments config ("--channel":arg:xs)      = resolveArguments (config { bot_chans       = bot_chans config ++ [arg] }) xs
resolveArguments config ("-C":arg:xs)             = resolveArguments (config { bot_commandDir  = arg }) xs
resolveArguments config ("--command":arg:xs)      = resolveArguments (config { bot_commandDir  = arg }) xs
resolveArguments config ("-L":arg:xs)             = resolveArguments (config { bot_logDir      = arg }) xs
resolveArguments config ("--log":arg:xs)          = resolveArguments (config { bot_logDir      = arg }) xs
resolveArguments config ("-S":arg:xs)             = resolveArguments (config { bot_scriptDir   = arg }) xs
resolveArguments config ("--script":arg:xs)       = resolveArguments (config { bot_scriptDir   = arg }) xs
resolveArguments config ("-N":arg:xs)             = resolveArguments (config { bot_ownerNick   = arg }) xs
resolveArguments config ("--ownerNick":arg:xs)    = resolveArguments (config { bot_ownerNick   = arg }) xs
resolveArguments config ("-U":arg:xs)             = resolveArguments (config { bot_ownerUser   = arg }) xs
resolveArguments config ("--ownerUser":arg:xs)    = resolveArguments (config { bot_ownerUser   = arg }) xs
resolveArguments _      ("-h":_)                  = error programUsage
resolveArguments _      ("--help":_)              = error programUsage
resolveArguments _      (x:_)                    = error ("Error: Incorrect Argument: "++x++"\n"++programUsage)


programUsage :: String
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








