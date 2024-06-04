module Main where
import Parser
import System.Environment   
import Data.List

--test = do  
--   args <- getArgs  
--   progName <- getProgName  
--   putStrLn "The arguments are:"  
--   mapM putStrLn args  
--   putStrLn "The program name is:"  
--   putStrLn progName  

--main :: IO ()
--main = do x <- getArgs
--          print (xpr (x !! 0))

main :: IO ()
main = do x <- getArgs
          print (xpr (x !! 0))
