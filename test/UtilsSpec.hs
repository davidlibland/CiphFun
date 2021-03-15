module UtilsSpec(spec) where

import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Numeric.Natural
import Data.List (sort)

import RailFence
import Utils

instance Arbitrary Natural where
  arbitrary = fromInteger <$> choose (1, 10)

spec :: Spec
spec = do
  describe "normalizing messages for ciphers" $ do
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
      prop "is the smallest multiple no smaller than the size" $ \multiples -> \n ->
        let bSize = foldl lcm 1 $ map fromIntegral (multiples :: [Natural]) in
        (computeLength multiples n) < n + bSize
  describe "transpose" $ do
      prop "is reversible" $ \(Positive n) -> \(Positive m) -> \letters -> let
          letterStream :: String
          letterStream = cycle (letters++"cat")
          matrix = [[letterStream !! (i+j)| i <- [1 .. m]]|j<- [1..n]]
        in (transpose . transpose) matrix == matrix
      it "is correct on [[1,2],[3,4]]" $
        transpose [[1,2], [3,4]] `shouldBe` [[1,3], [2,4]]
  describe "sortAgainst" $ do
      it "is correct on a simple example" $
        sortAgainst [3,2,4,1] "abcd" `shouldBe` "dbac"
      prop "works correctly" $ \lst ->
         sortAgainst lst lst == sort (lst :: String)
  describe "invRanks" $ do
      it "is correct on a simple example" $
        invRanks "bhue" `shouldBe` [1,4,2,3]
      prop "works correctly" $ \lst ->
         sortAgainst (invRanks lst) (sort lst) == (lst :: String)
  describe "ranks" $ do
      it "is correct on a simple example" $
        ranks "bhue" `shouldBe` [1,3,4,2]
      prop "works correctly" $ \lst ->
         sortAgainst (ranks lst) lst == sort (lst :: String)
