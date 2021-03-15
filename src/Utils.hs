module Utils (padC, padR, padM, prepString, padPrepStringC, padPrepStringM, oddEven, computeLength, transpose, sortAgainst, ranks, invRanks) where
  
import System.Random
import Control.Monad.State 
import Data.Char (toUpper)
import Data.List (sortOn)
import Numeric.Natural (Natural)

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

oddEven :: [a] -> ([a], [a])
oddEven [] = ([], [])
oddEven [x] = ([x], [])
oddEven (x:y:xs) = let (odds, evens) = oddEven xs in (x:odds, y:evens)


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [x] = map (: []) x
transpose (x:xs) = zipWith (:) x (transpose xs)

sortAgainst :: Ord a => [a] -> [b] -> [b]
sortAgainst ixs ys = let
    sortedTuples = sortOn fst $ zip ixs ys
  in map snd sortedTuples
  
invRanks :: Ord a => [a] -> [Int]
invRanks xs = sortAgainst xs [1..]

ranks :: Ord a => [a] -> [Int]
ranks = invRanks . invRanks

-- Computes the length of the encoded message. It must be the smallest
-- multiple of the multiples larger than size
computeLength :: [Natural] -> Int -> Int
computeLength multiples size = let 
  bSize = foldl lcm 1 $ map fromIntegral multiples  
  extra = size `mod` bSize
  in if extra == 0 then size else size + bSize - extra