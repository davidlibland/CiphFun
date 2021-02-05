module RailFenceSpec(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import RailFence
import Utils(prepString)
import Cipher

shouldMatch :: Either String String -> String -> Expectation
shouldMatch (Left x) _ = expectationFailure $ "Cipher failed: " ++ x
shouldMatch (Right decoded) original = 
  take (length $ prepString original) decoded `shouldBe` prepString original

shouldMatch_ :: Either String String -> String -> Bool
shouldMatch_ (Left _) _ = False
shouldMatch_ (Right decoded) original = take (length $ prepString original) decoded == prepString original

prop_roundTrip :: String -> RailFenceConfig -> Bool
prop_roundTrip msg config =
      (decode config =<< encode config msg)
        `shouldMatch_`
      msg
      
prop_goodBlocks :: String -> RailFenceConfig -> Bool
prop_goodBlocks msg config =
  case encode config msg of
      Left _ -> False
      Right codedMsg -> all (\block -> length block == fromIntegral (blocksize config)) codedMsg

instance Arbitrary RailFenceConfig where
  arbitrary = do
    n <- fromInteger <$> choose (1, 10)
    nullChoices <- listOf1 (choose ('a', 'z'))
    seed <- arbitrary
    return $ RailFenceConfig n nullChoices seed

spec :: Spec
spec = do
  describe "decoding encoded messages" $ do
    it "handles hi there" $
      (decode standardRailFenceConfig =<< encode standardRailFenceConfig "hi there")
      `shouldMatch`
      "hi there"
    prop "handles quickCheck messages" $ \msg ->
      (decode standardRailFenceConfig =<< encode standardRailFenceConfig msg)
        `shouldMatch`
      msg
    it "handles quickCheck messages and configs" $ property prop_roundTrip
    it "encodes messages with the correct blocksize" $ property prop_goodBlocks
