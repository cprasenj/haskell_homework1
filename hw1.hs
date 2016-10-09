module HW1 where
import qualified Data.List
import Debug.Trace

toDigits :: Integer -> [Integer]
toDigits num
  | num < 1 = []
  | otherwise = toDigits (quot num 10) ++ [rem num 10]
----------------------------------------------------------------------------------
nth :: [a] -> Int -> a
nth list index
  | length list < pred index = error "index out of bound"
  | otherwise = head . drop(index) $ list
----------------------------------------------------------------------------------
toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse $ toDigits num
----------------------------------------------------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = doubleEveryOther' [] list
  where
    doubleEveryOther' :: [Integer] -> [Integer] -> [Integer]
    doubleEveryOther' resultSoFar list
      | length list == 0 = reverse resultSoFar
      | length list < 2 =  reverse $ head list : resultSoFar
      | otherwise = doubleEveryOther' (((*) 2 $ nth list 1) : (head list) : resultSoFar) (drop 2 list)
