import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (sort)
import HW1

areEqual a b = sort a == sort b

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

  describe "nth" $ do

    it "shoud giev error for [] and 2" $ do
      nth [] 2 `shouldThrow` anyException

    it "should give 1 for [1] and 0" $ do
      nth [1] 0 `shouldBe` 1

  describe "doubleEveryOther" $ do

    it "should return [] for []" $ do
      doubleEveryOther []  `shouldBe` []

    it "should return [1] for [1]" $ do
      doubleEveryOther [1] `shouldBe` [1]

    it "should return [1 4 3 8] for [1 2 3 4]" $ do
      doubleEveryOther [1, 2, 3, 4] `shouldBe` [1, 4, 3, 8]

    it "should return [1 4 3] for [1 2 3]" $ do
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

  describe "sumDigits" $ do

    it "should return 15 for [1 2 3 4 5]" $ do
      sumDigits [1, 2, 3, 4, 5] `shouldBe` 15

    it "should return 20 for [1 4 3 16 5]" $ do
      sumDigits [1, 4, 3, 16, 5] `shouldBe` 20

    it "should return 0 for []" $ do
      sumDigits [] `shouldBe` 0
