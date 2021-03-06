{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module RailFence (RailFenceConfig(RailFenceConfig), blocksize, nulls, standardRailFenceConfig) where

import Cipher
import Data.List.Split (chunksOf)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Utils (padPrepStringM, oddEven, computeLength, prepString)
import System.Random (mkStdGen)
import Control.Monad.State.Lazy (evalState)
import Numeric.Natural (Natural)

data RailFenceConfig = RailFenceConfig {blocksize :: Natural, nulls :: String, seed :: Int} deriving (Show, Generic, FromJSON)

blocksize_ :: RailFenceConfig -> Int
blocksize_ = fromIntegral . blocksize

standardRailFenceConfig :: RailFenceConfig
standardRailFenceConfig = RailFenceConfig 4 "wkvxzjq" 3

instance Cipher RailFenceConfig where
  encode config msg = let
    n = computeLength [2, blocksize config] (length $ prepString msg)
    prepedMsg = evalState (padPrepStringM n (nulls config)  msg) (mkStdGen (seed config))
    (odds, evens) = oddEven prepedMsg
    in Right $ chunksOf (blocksize_ config) (odds++evens)
  decode config blocks = do
      encodedMsg <- if all (\block -> length block == blocksize_ config) blocks 
        then Right $ concat blocks
        else Left "Invalid Message Block sizes"
      if even (length encodedMsg) then Right () else Left "Encoded message must have even length"
      let (odds, evens) = splitAt (length encodedMsg `div` 2) encodedMsg
      let zippedMsg = zip odds evens
      return $ let 
          unFence [] = []
          unFence ((o, e):xs) = o:e: unFence xs
        in unFence zippedMsg
