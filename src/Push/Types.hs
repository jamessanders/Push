{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Push.Types where

import Data.Time
import Data.ByteString.Char8 (ByteString)
import System.FilePath ((</>))
import Text.RSTemplate

data SiteConfig = SiteConfig {
  getSiteName        :: ByteString,
  getSiteVersion     :: ByteString,
  getSiteDescription :: ByteString,
  getDocPath         :: FilePath,
  getBuildPath       :: FilePath,
  getTemplatePath    :: FilePath,
  getDateFormat      :: String  
  } deriving (Show)


defaultSiteConfig prefix = SiteConfig {
  getSiteName        = "",
  getSiteVersion     = "",
  getSiteDescription = "",
  getDocPath         = prefix </> "doc",
  getBuildPath       = prefix </> "build",
  getTemplatePath    = prefix </> "templates",
  getDateFormat      = "%B %e, %Y at %l:%M %p"
  }

data Article = Article {
  getCommitDate     :: UTCTime,
  getCommitAuthor   :: ByteString,
  getCommitEmail    :: ByteString,
  getLastCommitDate :: UTCTime,
  getFilePath       :: FilePath,
  getBody           :: String,
  getArticleTitle   :: String,
  getSiteConfig     :: SiteConfig
} deriving (Show)

newtype ArticleList a = ArticleList {
  unwrapAL :: [a] 
  } deriving (Show, Read)