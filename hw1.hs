module HW1 where
import qualified Data.List

toDigits :: Integer -> [Integer]
toDigits num
  | num < 1 = []
  | otherwise = toDigits (quot num 10) ++ [(rem num 10)]

----------------------------------------------------------------------------------
toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse(toDigits num)
----------------------------------------------------------------------------------

