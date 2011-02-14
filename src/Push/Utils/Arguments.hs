import Control.Monad.State
import System.Environment

data Arg = Arg String 
         | Flag String
         | EOA
         deriving (Show)

parseArgs args parser = evalState parser args

getNext :: State [String] Arg
getNext = do                                                
  args <- get                                               
  case args of                                              
    [] -> return EOA                                    
    _ -> do                                                 
      let c = head args                                     
      case take 2 c of                                      
        "--" -> do                                          
          put $ tail' args                                  
          return $ (Flag $ drop 2 c)                   
        _ -> case take 1 c of                               
          "-" -> do                                         
            let flag = head (drop 1 c)                      
            case tail' (drop 1 c) of                        
              [] -> put (tail' args )                       
              rest -> put (("-" ++ rest) : tail' args)      
            return $ (Flag [flag])                     
          _ -> do                                           
            put (tail' args)                                
            return $ (Arg $ head args)                 
  where                                                            
    tail' (x:xs) = xs
    tail' []     = []

  
getRest :: State [String] [String]
getRest = do                                                
  l <- get                                                  
  put []                                                    
  return l                                                  


{-
main = do
  args <- getArgs
  let arg = parseArgs args parser
  print arg
  where
    parser = do
      next <- getNext
      case next of
        Flag "v" -> parser >>= return . ("Verbose" :)
        Flag "t" -> parser >>= return . ("Text" :)
        Flag "o" -> do
          n <- getNext
          case n of
            Arg x -> do
              rest <- parser 
              return ("Out" : x : rest)
            _ -> error "Invalid Argument"
        Flag _   -> error "Invalid Argument"
        Arg  _   -> error "Invalid Argument"
        EOA      -> return []
-}