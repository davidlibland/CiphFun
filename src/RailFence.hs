module RailFence (RailFenceConfig(RailFenceConfig), blocksize, nulls, standardRailFenceConfig) where

import Cipher
import Data.List.Split (chunksOf)
import Utils (padPrepStringM, oddEven, computeLength, prepString)
import System.Random (mkStdGen)
import Control.Monad.State.Lazy (evalState)
import Numeric.Natural (Natural)

data RailFenceConfig = RailFenceConfig {blocksize :: Natural, nulls :: String, seed :: Int} deriving (Show)

standardRailFenceConfig :: RailFenceConfig
standardRailFenceConfig = RailFenceConfig 4 "wkvxzjq" 3

instance Cipher RailFenceConfig where
  encode config msg = let
    n = computeLength [2, blocksize config] (length $ prepString msg)
    prepedMsg = evalState (padPrepStringM n (nulls config)  msg) (mkStdGen (seed config))
    (odds, evens) = oddEven prepedMsg
    in Right $ chunksOf (fromIntegral $ blocksize config) (odds++evens)
  decode config blocks = let
    encodedMsgM = if all (\block -> length block == fromIntegral (blocksize config)) blocks
      then Right $ concat blocks else Left "Invalid Message Block sizes"
    zippedMsg = fmap (\encodedMsg -> 
        let 
          (odds, evens) = splitAt (length encodedMsg `div` 2) encodedMsg
        in zip odds evens) encodedMsgM
    unFence [] = []
    unFence ((o, e):xs) = o:e: unFence xs
    in fmap unFence zippedMsg
