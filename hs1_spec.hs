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
  describe "validate" $ do

    it "should give true for  4012888888881881" $ do
      validate 4012888888881881 `shouldBe` True

    it "should give true for  4012888888881882" $ do
      validate 4012888888881882 `shouldBe` False

  describe "hanoi" $ do

    it "should give [(a, c), (a, b), (c, b)] for 2, a, b, c" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
