module CodeWord (CodeWordConfig(CodeWordConfig), blocksize, standardCodeWordConfig) where

import Cipher
import Data.List.Split (chunksOf)
import Utils (padPrepStringM, computeLength, prepString, sortAgainst, invRanks, transpose)
import System.Random (mkStdGen)
import Control.Monad.State.Lazy (evalState)
import Numeric.Natural (Natural)

data CodeWordConfig = CodeWordConfig {codeword:: String, blocksize :: Natural, nulls :: String, seed :: Int} deriving (Show)

blocksize_ = fromIntegral . blocksize

standardCodeWordConfig :: CodeWordConfig
standardCodeWordConfig = CodeWordConfig "enigma" 4 "wkvxzjq" 3


encodeChunk :: CodeWordConfig -> String -> Either String [String]
encodeChunk config block = let
    cwsize = length $ codeword config
    transposedMsg = transpose $ chunksOf cwsize block
  in Right $ sortAgainst (codeword config) transposedMsg
  
decodeChunk :: CodeWordConfig -> [String] -> Either String String
decodeChunk config block = Right $ concat $ transpose $ sortAgainst (invRanks $ codeword config) block

instance Cipher CodeWordConfig where
  encode config msg = let
    chunkSize = blocksize_ config * length (codeword config)
    n = computeLength [fromIntegral chunkSize] (length $ prepString msg)
    prepedMsg = evalState (padPrepStringM n (nulls config)  msg) (mkStdGen (seed config))
    msgBlocks :: [String]
    msgBlocks = chunksOf chunkSize prepedMsg
    encodedBlocksE :: [Either String [String]]
    encodedBlocksE = map (encodeChunk config) msgBlocks
    engage :: [Either String [String]] -> Either String [String]
    engage = fmap concat . sequence
    in engage encodedBlocksE
  decode config = fmap concat . mapM (decodeChunk config) . chunksOf (length $ codeword config)