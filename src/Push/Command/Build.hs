{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
module Push.Command.Build (main) where
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
import Text.Twine
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BC

instance (Monad m) => TemplateInterface m String where
  makeString = return


instance TemplateInterface IO SiteConfig where
  property "name" = return . bind . getSiteName
  property "description" = return . bind . getSiteDescription
  property _      = \x -> return $ bind ("" :: String)

instance TemplateInterface IO Article where
  property "title"      = return . bind . getArticleTitle
  property "commitDate" = \x -> ((makeDate (getDateFormat . getSiteConfig $ x) $ getCommitDate x) >>= return . bind)
  property "lastUpdate" = \x -> ((makeDate (getDateFormat . getSiteConfig $ x) $ getLastCommitDate x) >>= return . bind)
  property "body"       = return . bind . getBody
  property "path"       = return . bind . (++ ".html") . dropExtension . takeFileName . getFilePath
  property _            = \x -> return $ bind ("" :: String)

instance TemplateInterface IO ArticleList where
  property "reverse" = return . bind . reverse . M.elems . unwrapAL
  property "keys"    = return . bind . M.keys . unwrapAL
  --property "take"    = \al -> return . ContextFunction $ (\([ContextInteger x])-> return . bind $ ArticleList (take (fromIntegral x) (unwrapAL al)))
  property "get" = \a -> 
    let get k = do
          key <- unbind k
          return $ bind $ M.lookup key (unwrapAL a)
    in return . method $ \ls -> mapM get ls >>= return . bind
  property "" = \_-> return $ bind ("failure" :: String)
  makeIterable = return . map bind . M.elems . unwrapAL


makeArticleList = ArticleList . foldl aux M.empty 
  where 
    aux a b = M.insert (getSlug b) b a

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
        getSlug           = (takeBaseName path),
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
    "articles" =: (makeArticleList articles)
    "site"     =: siteConfig
  index <- evalTemplate (cwd </> getTemplatePath siteConfig </> "index.html") context 
  let bIndex = (cwd </> getBuildPath siteConfig </> "index.html")
  catch (createDirectory (cwd </> getBuildPath siteConfig)) (\_-> return ())
  BC.writeFile bIndex index
  
  forM_ articles $ \article -> do
    let writeTo = replaceExtension (cwd </> getBuildPath siteConfig </> (makeRelative (cwd </> getDocPath siteConfig) (getFilePath article))) $ "html"
    context <- makeContext $ do
      "article"  =: article
      "articles" =: (makeArticleList articles)
      "site"     =: siteConfig
    art <- evalTemplate (cwd </> getTemplatePath siteConfig </> "article.html") context 
    BC.writeFile writeTo art