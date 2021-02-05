module UtilsSpec(spec) where

import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Numeric.Natural

import RailFence
import Utils

instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (1, 10)

spec :: Spec
spec = do
  describe "normalizing messages for ciphers" $ do
--    it "handles hi there" $
--      decode standardRailFenceConfig (encode standardRailFenceConfig "hi there")
--      `shouldMatch`
--      "hi there"
    prop "padC produces the correct size message" $ \msg -> \(Positive n) ->
      length (padC "xywz" n msg)
        `shouldBe`
      max n (length msg)
    prop "padR produces the correct size message" $ \msg -> \(Positive n) ->
      length (padR (mkStdGen 5) "xywz" n msg)
        `shouldBe`
      max n (length msg)
  describe "test that oddEven correctly splits lists" $ do
      prop "splits lists in half" $ \msg  ->
        let (odds, evens) = oddEven (msg :: String) in
          ((length evens <= length odds) && (length odds <= (1 + length evens)))
      prop "preserves list length" $ \msg  ->
        let (odds, evens) = oddEven (msg :: String) in
          length odds + length evens == length msg
  describe "test that computeLength" $ do
      prop "produces a multiple of all the multiples" $ \multiples -> \n ->
        let bSize = foldl lcm 1 $ map fromIntegral (multiples :: [Natural]) in
        (computeLength multiples n) `mod` bSize == 0