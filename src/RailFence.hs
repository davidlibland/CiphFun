module RailFence (RailFenceConfig(RailFenceConfig), blocksize, nulls, standardRailFenceConfig) where

import Cipher
import Data.List.Split (chunksOf)
import Utils (padPrepStringM)
import System.Random (mkStdGen)
import Control.Monad.State.Lazy (evalState)
import Numeric.Natural (Natural)

data RailFenceConfig = RailFenceConfig {blocksize :: Natural, nulls :: String, seed :: Int} deriving (Show)

standardRailFenceConfig :: RailFenceConfig
standardRailFenceConfig = RailFenceConfig 4 "wkvxzjq" 3

oddEven :: [a] -> ([a], [a])
oddEven [] = ([], [])
oddEven [x] = ([x], [])
oddEven (x:y:xs) = let (odds, evens) = oddEven xs in (x:odds, y:evens)

-- Computes the length of the encoded message. It must be the smallest
-- even multiple of blocksize which is longer than the size
computeLength :: Natural -> Int -> Int
computeLength blockSize size = let 
  try = size + (size `mod` fromIntegral blockSize)
  in if even try then try else try + fromIntegral blockSize 

instance Cipher RailFenceConfig where
  encode config msg = let
    n = computeLength (blocksize config) (length msg)
    prepedMsg = evalState (padPrepStringM n (nulls config)  msg) (mkStdGen (seed config))
    (odds, evens) = oddEven prepedMsg
    in chunksOf (fromIntegral $ blocksize config) (odds++evens)
  decode _ blocks = let
    encodedMsg = concat blocks
    (odds, evens) = splitAt (length encodedMsg `div` 2) encodedMsg
    zippedMsg = zip odds evens
    unFence [] = []
    unFence ((o, e):xs) = o:e: unFence xs
    in unFence zippedMsg
