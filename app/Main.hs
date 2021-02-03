module Main where

import RailFence
import Cipher

main :: IO ()
main = do  
   putStrLn "What message would you like to encode?"  
   msg <- getLine  
   putStrLn $ unwords $ encode standardRailFenceConfig msg
