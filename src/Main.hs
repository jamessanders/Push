import System.Environment
import System.Exit

import qualified Push.Command.Build
import qualified Push.Command.Init

-- Maps subcommand strings to there corresponsing modules/functions
getSubProcess "build" = Just Push.Command.Build.main
getSubProcess "init"  = Just Push.Command.Init.main
getSubProcess _ = Nothing

usage = do
  mapM_ putStrLn [
    "\nUsage: push <subcommand> [arguments]",
    "",
    "where <subcommand> is one of: ",
    "\n\t build: Build a static site",
    ""
    ]
  exitFailure
  
  
getSubCommand = do
  args <- getArgs 
  return $ if length args < 1 then "" else head args
  
main = do
  subcommand <- getSubCommand 
  case getSubProcess subcommand of
    Nothing -> usage
    Just proc -> proc
