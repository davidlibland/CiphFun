module CodeWordSpec(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeWord
import Utils(prepString)
import Cipher

gardnerConfig = CodeWordConfig "frank" 4 "wxqz" 3

shouldMatch :: Either String String -> String -> Expectation
shouldMatch (Left x) _ = expectationFailure $ "Cipher failed: " ++ x
shouldMatch (Right decoded) original = 
  take (length $ prepString original) decoded `shouldBe` prepString original

shouldMatch_ :: Either String String -> String -> Bool
shouldMatch_ (Left _) _ = False
shouldMatch_ (Right decoded) original = take (length $ prepString original) decoded == prepString original

prop_roundTrip :: String -> CodeWordConfig -> Bool
prop_roundTrip msg config =
      (decode config =<< encode config msg)
        `shouldMatch_`
      msg
      
prop_goodBlocks :: String -> CodeWordConfig -> Bool
prop_goodBlocks msg config =
  case encode config msg of
      Left _ -> False
      Right codedMsg -> all (\block -> length block == fromIntegral (blocksize config)) codedMsg

instance Arbitrary CodeWordConfig where
  arbitrary = do
    codeword <- listOf1 (choose ('a', 'z'))
    n <- fromInteger <$> choose (1, 10)
    nullChoices <- listOf1 (choose ('a', 'z'))
    seed <- arbitrary
    return $ CodeWordConfig codeword n nullChoices seed

spec :: Spec
spec = do
  describe "decoding encoded messages" $ do
    it "handles 'meet me thursday night'" $ 
      (encode gardnerConfig "Meet me thursday night x") `shouldBe` Right ["EHAH", "MESI", "MRNX", "TUYT", "ETDG"]
    it "handles hi there" $
      (decode standardCodeWordConfig =<< encode standardCodeWordConfig "hi there")
      `shouldMatch`
      "hi there"
    prop "handles quickCheck messages" $ \msg ->
      (decode standardCodeWordConfig =<< encode standardCodeWordConfig msg)
        `shouldMatch`
      msg
    it "handles quickCheck messages and configs" $ property prop_roundTrip
    it "encodes messages with the correct blocksize" $ property prop_goodBlocks


