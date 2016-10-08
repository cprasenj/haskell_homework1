-- import qualified Data.List as DL
module HW1 where


-- toDigits :: Int -> [Int]
-- toDigits n = toDigits' n []
--   where
--     toDigits' :: Int -> [Int] -> [Int]
--     toDigits' num resultSoFar =
--       if num == 0
--         then resultSoFar
--         else toDigits' (quot num 10) ((rem num 10) : resultSoFar)

-- Another way
toDigits :: Integer -> [Integer]
toDigits num
  | num < 1 = []
  | otherwise = toDigits (quot num 10) ++ [(rem num 10)]

----------------------------------------------------------------------------------

-- toDigitsRev :: Integer -> [Integer]
-- toDigitsRev 0 = []
-- toDigitsRev num = [(rem num 10)] ++ toDigitsRev (quot num 10)

-- Another way
-- toDigitsRev :: Integer -> [Integer]
-- toDigitsRev num = DL.reverse(toDigits num)
----------------------------------------------------------------------------------

