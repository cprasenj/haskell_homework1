import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HW1

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do
    it "should return [] for 0" $ do
      toDigits 0  `shouldBe` []

    it "should return [] for -1" $ do
      toDigits (-1) `shouldBe` []

    it "should return [1 2 3] for 123" $ do
      toDigits (123) `shouldBe` [1, 2, 3]

  describe "toDigitsRev" $ do
    it "should return [] for 0" $ do
      toDigitsRev 0  `shouldBe` []

    it "should return [] for -1" $ do
      toDigitsRev (-1) `shouldBe` []

    it "should return [3 2 1] for 123" $ do
      toDigitsRev (123) `shouldBe` [3, 2, 1]
