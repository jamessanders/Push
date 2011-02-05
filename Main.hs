{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
import System.FilePath.Find
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Posix.IO
import System.Posix.Files
import Data.Time
import Data.Time.Clock.POSIX
import Text.RSTemplate
import Data.ByteString.Char8 (ByteString)
import System.IO
import GHC.Exts (sortWith)
import Text.Pandoc
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as BC

data Site = Site {
  getTitle :: ByteString
  }

data Article = Article {
  getCommitDate   :: UTCTime,
  getCommitAuthor :: ByteString,
  getCommitEmail  :: ByteString,
  getFilePath     :: FilePath,
  getBody         :: String,
  getArticleTitle :: String
} deriving (Show)

instance (Monad m) => ContextBinding m Site where
  binding "title" = return . bind . getTitle

instance ContextBinding IO Article where
  binding "title"      = return . bind . getArticleTitle
  binding "commitDate" = return . bind . show . getCommitDate
  binding "body"       = return . bind . getBody
  binding _            = \x -> return $ bind ("" :: String)

docPath      = "doc"
buildPath    = "build"
templatePath = "templates"

markdownToHtml :: FilePath -> IO String
markdownToHtml path = do
  st <- readFile path
  let pan = readMarkdown defaultParserState st
  return $ writeHtmlString defaultWriterOptions pan

findTitle str = 
  let tags   = parseTags str
      lead   = takeWhile (not . isHeaderOpen) tags
      rest   = tail $ dropWhile (not . isHeaderOpen) tags
  in (t2s (head rest), renderTags $ lead ++ (tail $ dropWhile (not . isHeaderClose) rest))
  where 
    isHeaderOpen (TagOpen x _) | x == "h1" || x == "h2" || x == "h3" = True
    isHeaderOpen _ = False
    isHeaderClose (TagClose x) | x == "h1" || x == "h2" || x == "h3" = True
    isHeaderClose _ = False

    t2s (TagText txt) = txt
    t2s x = show x

create path = do
  fd <- createFile path regularFileMode 
  fdWrite fd "X"
  closeFd fd

git args = do
  (_, Just hout, _, ph) <- createProcess (proc "git" args){ std_out = CreatePipe }
  ec <- waitForProcess ph
  BC.hGetContents hout

getArticleInfo path = do
  [date,email,author] <- git ["log", "--format=%at:%ae:%an", path] >>= 
                           return . BC.split ':' . last . BC.lines
  content <- markdownToHtml path
  let (title, content') = findTitle content
  return $  Article {
    getFilePath     = path,
    getCommitDate   = posixSecondsToUTCTime (realToFrac $ read $ BC.unpack date),
    getCommitEmail  = email,
    getCommitAuthor = author,
    getBody         = content',
    getArticleTitle = title
    }

getAllArticleFiles path = do
  find (return True) (fileType ==? RegularFile) path

main = do
  cwd      <- getCurrentDirectory
  articles <- getAllArticleFiles (cwd </> docPath) >>= mapM getArticleInfo 
  context <- makeContext $ do
    set "articles" (reverse $ sortWith (getCommitDate) articles)
    set "site" (Site "Test Blog")
  index <- evalTemplate (cwd </> templatePath </> "index.html") context 
  let bIndex = (cwd </> buildPath </> "index.html")
  catch (createDirectory (cwd </> buildPath)) (\_-> return ())
  BC.writeFile bIndex index
      