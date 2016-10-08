module HW1 where
import qualified Data.List
import Debug.Trace

toDigits :: Integer -> [Integer]
toDigits num
  | num < 1 = []
  | otherwise = toDigits (quot num 10) ++ [rem num 10]

----------------------------------------------------------------------------------
toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse(toDigits num)
----------------------------------------------------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = doubleEveryOther' [] list
  where
    doubleEveryOther' :: [Integer] -> [Integer] -> [Integer]
    doubleEveryOther' resultSoFar list
      | length list == 0 = resultSoFar
      | length list < 2 = head list : resultSoFar
      | otherwise = doubleEveryOther' (((list !! 1) * 2) : (head list) : resultSoFar) (drop 2 list)
