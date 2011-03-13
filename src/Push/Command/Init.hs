module Push.Command.Init where
  
import System.Environment
import System.Directory
import System.FilePath
import Paths_Push
import Text.Twine
import Data.Char
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Push.Trans

mkdir path = do 
  exists <- doesDirectoryExist path
  case exists of
    False -> do
      putStrLn $ " + Creating directory " ++ path
      createDirectory path
      return True
    True -> do
      putStrLn $ " - Directory '" ++ path ++ "' already exist skipping"
      return False

askYesNo ask = do
  putStrLn $ ask ++ " [y/n]"
  answer <- getChar
  case toUpper answer of
    'Y' -> return True
    'N' -> return False
    _   -> do
      putStrLn "You must anwer with 'y' (yes) or 'n' (no)"
      askYesNo ask

maybeWrite what path = do
  exist <- doesFileExist path
  case exist of
    True -> putStrLn $ " - " ++ path ++ " already exists, skipping."
    False -> do
      BS.writeFile path what
       

printLines lines =
  putStrLn (unlines lines)

getLine' pre def = do
  putStrLn ""
  putStrLn pre
  line <- getLine
  if line == "" 
    then return def 
    else return line
  
makeInitialSiteConf context = do
  tmpl <- getDataFileName "extra/site.conf.tmpl"
  evalTemplate tmpl context

main = do
  cwd <- getCurrentDirectory >>= canonicalizePath
  putStrLn "\nIntializing new push project\n"
  printLines ["This will initializing push project in current directory",
              "you will be prompted before any files get overwritten."]
  putStrLn "------------------------------------------------------------------------"
  
  title       <- getLine' "What will the name of this site be?" "Untitled"
  desc        <- getLine' "Please describe this site (in one line)..." ""
  docsdir     <- getLine' "Where shall the source documents be located? [default: ./docs]" "docs"
  builddir    <- getLine' "Where shall the site be build? [default: ./build]" "build"
  templatedir <- getLine' "Where shall the template documents be located? [default: ./templates]" "templates"
  mediadir    <- getLine' "Where shall the media documents be located? [default: ./media]" "media"
    
  context <- makeContext $ do
    "name"           =: title
    "desc"           =: desc
    "src-path"      =: docsdir
    "build-path"     =: builddir
    "templates-path" =: templatedir
    "media-path"     =: mediadir
  
  siteconf <- makeInitialSiteConf context 
  
  putStrLn "Site Config:"
  putStrLn "------------\n"
  BS.putStrLn siteconf
  ans <- askYesNo "\nDoes this look right to you?"
  when (ans == True) $ do
    mkdir $ cwd </> docsdir
    mkdir $ cwd </> templatedir
    mkdir $ cwd </> mediadir
    maybeWrite siteconf "site.conf"
    return ()
    
  putStrLn "\nDone."
  




