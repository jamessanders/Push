{-# LANGUAGE OverloadedStrings #-}

module Push.Configs where

import Push.Types
import System.Directory
import System.FilePath
import Data.Object
import Data.Object.Yaml
import Data.Maybe
import Push.Utils

buildSiteConfig path = do
  let dc = defaultSiteConfig (takeDirectory path)
  fe <- doesFileExist path
  if not fe 
    then return dc
    else do
      config <- decodeFile path
      case config of
        Just conf -> 
          processConfig dc conf
        Nothing -> return dc
  where 
    processConfig site config = do
      let cwd = takeDirectory path
      return $ site {                                 
        getSiteName        = lookupConf "" "Name",              
        getSiteVersion     = lookupConf "0.0.1" "Version",    
        getSiteDescription = lookupConf "" "Description",  
        getDocPath         = cwd </> (unsafeUnpackBS $ lookupConf "doc" "src-path")
        }                                             
      where
        lookupConf def = fromMaybe def . lookupConf'
        lookupConf' str = (fromMapping config) >>= lookupScalar (str :: String)


