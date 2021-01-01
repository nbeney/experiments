import System.Environment   
import Data.List  
  
main = do  
  args <- getArgs  
  progName <- getProgName
  putStrLn $ "The arguments are: " ++ show args 
  mapM putStrLn args  
  putStrLn $ "The program name is: " ++ progName  
