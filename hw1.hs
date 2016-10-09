module HW1 where
import qualified Data.List
import Debug.Trace
type Peg = String
type Move = (Peg, Peg)

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
toDigitsRev num = reverse . toDigits $ num
----------------------------------------------------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = map (\ pair -> fst pair $ snd pair) (zip (cycle [(*) 1, (*) 2]) list)

-- Pooja and Prasenjit's idiot concat thing
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther list
--   | list == [] = []
--   | length list == 1 = list
--   | length list <= 2 = (head list) : [2 * (list !! 1)]
--   | otherwise = (doubleEveryOther (take 2 list)) ++ (doubleEveryOther (drop 2 list))
---------------------------------------------------------------------------------

sumDigits :: [Integer] -> Integer
sumDigits listOfNumbers = foldl (\result num -> (+) result $ sum $ toDigits num) 0 listOfNumbers
---------------------------------------------------------------------------------

validate :: Integer -> Bool
validate creditCardNumber = (== 0) . flip rem 10 . sumDigits . doubleEveryOther . toDigitsRev $ creditCardNumber
---------------------------------------------------------------------------------

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi num peg1 peg2 peg3
  | num < 1 = []
  | otherwise = hanoi numberOfPegsLeft peg1 peg3 peg2 ++ [(peg1, peg2)] ++ hanoi numberOfPegsLeft peg3 peg2 peg1
  where
    numberOfPegsLeft = pred num
