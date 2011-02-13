{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
import Control.Failure
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Object
import Data.Object.Yaml
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format
import GHC.Exts (sortWith)
import Push.Types
import Push.Utils
import System.Directory
import System.Environment
import System.FilePath
import System.FilePath.Find
import System.IO
import System.Locale
import System.Posix.Files
import System.Posix.IO
import Text.HTML.TagSoup
import Text.Pandoc
import Text.RSTemplate
import qualified Data.ByteString.Char8 as BC

instance (Monad m) => ContextBinding m SiteConfig where
  binding "name" = return . bind . getSiteName
  binding "description" = return . bind . getSiteDescription
  binding _      = \x -> return $ bind ("" :: String)

instance ContextBinding IO Article where
  binding "title"      = return . bind . getArticleTitle
  binding "commitDate" = \x -> ((makeDate (getDateFormat . getSiteConfig $ x) $ getCommitDate x) >>= return . bind)
  binding "lastUpdate" = \x -> ((makeDate (getDateFormat . getSiteConfig $ x) $ getLastCommitDate x) >>= return . bind)
  binding "body"       = return . bind . getBody
  binding "path"       = return . bind . (++ ".html") . dropExtension . takeFileName . getFilePath
  binding _            = \x -> return $ bind ("" :: String)

instance ContextBinding IO (ArticleList Article) where
  binding "reverse" = return . bind . ArticleList . reverse . unwrapAL
  binding "take"    = \al -> return . ContextFunction $ (\([ContextInteger x])-> return . bind $ ArticleList (take (fromIntegral x) (unwrapAL al)))
  binding "" = \_-> return $ bind ("failure" :: String)
  makeIterable = return . map bind . unwrapAL


makeDate :: String -> UTCTime -> IO String
makeDate dateFormat d = do
  tz <- getCurrentTimeZone 
  return $ formatTime defaultTimeLocale dateFormat $ utcToLocalTime tz d

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


buildArticlesList siteConfig = 
  getAllArticleFiles (getDocPath siteConfig) >>=
  mapM getArticleInfo
  where
    getArticleInfo path = do                                                           
      log <- git ["log", "--format=%at:%ae:%an", path]
      let [date,email,author] = BC.split ':' . last . BC.lines $ log
      let [lastCommit,_,_] = BC.split ':' . head . BC.lines $ log
      content <- markdownToHtml path                                                   
      let (title, content') = findTitle content                                        
      return $  Article {                                                              
        getFilePath       = path,                                                        
        getCommitDate     = posixSecondsToUTCTime (realToFrac $ read $ BC.unpack date),  
        getLastCommitDate = posixSecondsToUTCTime (realToFrac $ read $ BC.unpack lastCommit),  
        getCommitEmail    = email,                                                       
        getCommitAuthor   = author,                                                      
        getBody           = content',                                                    
        getArticleTitle   = title,                                                        
        getSiteConfig     = siteConfig
                          
        }                                                                              
                                                                                       
    getAllArticleFiles path = do                                                       
      find (return True) (fileType ==? RegularFile) path                               

buildSiteConfig path = do
  config <- decodeFile path
  case config of
    Just conf -> do
      processConfig (defaultSiteConfig (takeDirectory path)) conf
    Nothing -> return (defaultSiteConfig (takeDirectory path))
  where 
    processConfig site config =                     
      return $ site {                                 
        getSiteName        = lookupConf "" "Name",              
        getSiteVersion     = lookupConf "0.0.1" "Version",    
        getSiteDescription = lookupConf "" "Description"  
        }                                             
      where
        lookupConf def = fromMaybe def . lookupConf'
        lookupConf' str = (fromMapping config) >>= lookupScalar (str :: String)



main = do
  cwd        <- getCurrentDirectory
  siteConfig <- buildSiteConfig (cwd </> "site.conf")
  articles   <- buildArticlesList siteConfig
  context    <- makeContext $ do
    set "articles" (ArticleList $ reverse $ sortWith (getCommitDate) articles)
    set "site" siteConfig
    set "reverse" $ ContextFunction $ \[(ContextList x)]-> return (ContextList $ reverse x) :: IO (ContextItem IO)
  index <- evalTemplate (cwd </> getTemplatePath siteConfig </> "index.html") context 
  let bIndex = (cwd </> getBuildPath siteConfig </> "index.html")
  catch (createDirectory (cwd </> getBuildPath siteConfig)) (\_-> return ())
  BC.writeFile bIndex index
  
  forM_ articles $ \article -> do
    let writeTo = replaceExtension (cwd </> getBuildPath siteConfig </> (makeRelative (cwd </> getDocPath siteConfig) (getFilePath article))) $ "html"
    context <- makeContext $ do
      set "article" article
      set "articles" (ArticleList $ reverse $ sortWith (getCommitDate) articles)
      set "site" siteConfig
    art <- evalTemplate (cwd </> getTemplatePath siteConfig </> "article.html") context 
    BC.writeFile writeTo art