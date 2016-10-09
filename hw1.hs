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
toDigitsRev num = reverse . toDigits $ num
----------------------------------------------------------------------------------
-- explanation
-- cycle creates an infinite loop so (cycle [((*) 1), ((*) 2)]) will create an infinite list
-- of functions (*)1, (*)2, (*)1....
-- now say the input list is [1,2,3,4]
-- (zip (cycle [((*) 1), ((*) 2)]) list) will create a list of paires of size of you list
-- in this case it will be [(((*)1), 1), (((*)2), 2), (((*)3), 1),(((*)4), 2)]
-- $ is a combinator it takes a function and applies it to a value
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- uncurry is needed to match the signature of the map

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = map (uncurry ($))(zip (cycle [((*) 1), ((*) 2)]) list)
