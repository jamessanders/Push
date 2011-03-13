module Push.Configs where

import Push.Types

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
    processConfig site config =                     
      return $ site {                                 
        getSiteName        = lookupConf "" "Name",              
        getSiteVersion     = lookupConf "0.0.1" "Version",    
        getSiteDescription = lookupConf "" "Description"  
        }                                             
      where
        lookupConf def = fromMaybe def . lookupConf'
        lookupConf' str = (fromMapping config) >>= lookupScalar (str :: String)


