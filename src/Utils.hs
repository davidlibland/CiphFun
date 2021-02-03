module Utils (padC, padR, padM, prepString, padPrepStringC, padPrepStringM) where
  
import System.Random
import Control.Monad.State 
import Data.Char (toUpper)

padFromStream :: [Char] -> Int -> String -> String
padFromStream nullStream n msg = msg ++ take (n - length msg) nullStream

padR :: StdGen -> [Char] -> Int -> String -> String
padR g nulls n msg = let 
    nullStream = map (nulls !!) (randomRs (0, length nulls - 1) g)
  in padFromStream nullStream n msg
  
padM ::  [Char] -> Int -> String -> State StdGen String
padM nulls n msg = state $ \g -> (padR g nulls n msg, g)

padC :: [Char] -> Int -> String -> String
padC nulls = padFromStream (cycle nulls)

alphaNumericChars :: [Char]
alphaNumericChars = ['0'..'9']++['A'..'Z']++['a'..'z']

isAlphaNumeric :: Char -> Bool
isAlphaNumeric x = x `elem` alphaNumericChars

prepString :: String -> String
prepString = map toUpper . filter isAlphaNumeric 

padPrepStringC :: Int -> [Char] -> String -> String
padPrepStringC n nulls = prepString  . padC nulls n . prepString

padPrepStringM :: Int -> [Char] -> String -> State StdGen String
padPrepStringM n nulls = fmap prepString . padM nulls n . prepString