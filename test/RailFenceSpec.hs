module RailFenceSpec(rfSpec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import RailFence
import Utils(prepString)
import Cipher

shouldMatch :: String -> String -> Expectation
shouldMatch decoded original = take (length $ prepString original) decoded `shouldBe` prepString original

shouldMatch_ :: String -> String -> Bool
shouldMatch_ decoded original = take (length $ prepString original) decoded == prepString original

prop_roundTrip :: String -> RailFenceConfig -> Bool
prop_roundTrip msg config =
      decode config (encode config msg)
        `shouldMatch_`
      msg

instance Arbitrary RailFenceConfig where
  arbitrary = do
    n <- fromInteger <$> choose (1, 10)
    nullChoices <- listOf1 (choose ('a', 'z'))
    seed <- arbitrary
    return $ RailFenceConfig n nullChoices seed

rfSpec :: Spec
rfSpec = do
  describe "decoding encoded messages" $ do
    it "handles hi there" $
      decode standardRailFenceConfig (encode standardRailFenceConfig "hi there")
      `shouldMatch`
      "hi there"
    prop "handles quickCheck messages" $ \msg ->
      decode standardRailFenceConfig (encode standardRailFenceConfig msg)
        `shouldMatch`
      msg
    it "handles quickCheck messages and configs" $ property prop_roundTrip
