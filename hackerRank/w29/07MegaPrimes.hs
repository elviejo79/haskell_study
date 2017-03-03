import Control.Monad
import System.Random
import System.IO.Unsafe
import Numeric
import Data.List
import Data.Maybe
import Data.Char

candidates :: Int -> [Integer]
candidates magnitude =
  let
    previousMagnitude = magnitude -1
    cs= replicateM previousMagnitude [2,3,5,7]

  in
    --foldl (\newList ns -> appendDigits ns ++ newList ) []  $ replicateM previousMagnitude [2,3,5,7]
    --filter (not.isDivisibleByPrimes) $ map unDigits $  replicateM previousMagnitude [2,3,5,7]
    --map unDigits $ filter (not.divisibleByCombined) $ replicateM previousMagnitude [2,3,5,7]
    --filter (not.isDivisibleByPrimes) $ map unDigits $  filter (not.divisibleByCombined) [c++[t] | c<-cs,t<-[3,7]]
    filterPrimes $ map unDigits $  divisibleByCombined [c++[t] | c<-cs,t<-[3,7]]


    
appendDigits :: [Int]->[Integer]
appendDigits digits =
  let
    ns = unDigits digits
    n3 = ns*10+3
    n7 = ns*10+7
    res = (if (not $ isDivisibleByPrimes n3) then res ++ [n3] else []) ++(if (not $isDivisibleByPrimes n7) then [n7] else [])
  in
    res
  


rulesIsDivisibleByPrimes = map isDivisibleBy [11,17,19,23] --[2,3,5,7,11,13,17,19,23]

isDivisibleByPrimes :: Integer -> Bool
isDivisibleByPrimes ns = or $ map ($ ns) rulesIsDivisibleByPrimes


isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy 2 numerator = even numerator
isDivisibleBy denominator numerator = 0 == flip mod denominator numerator


divisibleBy :: Int -> [Int] -> Bool
divisibleBy 2 digits = last digits==2
divisibleBy 3 digits = (sum digits) `mod` 3 == 0
divisibleBy 5 digits = (last digits) == 0
divisibleBy 7 digits = (sum $ zipWith (*) (reverse digits) (cycle [1,3,2,6,4,5])) `mod` 7 == 0
--divisibleBy 11 ds = remainderMethod 11 (-1) ds
divisibleBy 13 digits = (sum $ zipWith (*) (reverse digits) (cycle [1,10,9,12,3,4])) `mod` 13 == 0
--divisibleBy 13 ds = remainderMethod 13 (4) ds
-- divisibleBy 17 ds = remainderMethod 17 (-5) ds
-- divisibleBy 19 ds = remainderMethod 19 2 ds
-- divisibleBy 23 ds = remainderMethod 23 7 ds



--divisibleByCombined digits= or $ map ($ digits) $ map divisibleBy [2,3,5,7,13]
divisibleByCombined :: [[Int]] ->[[Int]]
--divisibleByCombined digits= filter (not.divisibleBy 13) $ filter (not.divisibleBy 7) $ filter (not.divisibleBy 5) $ filter (not.divisibleBy 3) $ filter (not.divisibleBy 2) digits
divisibleByCombined = (chainFilters filters1)

filters1 = map (divisibleBy) [2,3,5,7]
chainFilters :: [(a -> Bool)]->[a]->[a]
chainFilters [] ds = ds
chainFilters fs ds = filter (not.(head fs)) $ chainFilters (tail fs) ds

filterPrimes ds = filter (not.isDivisibleBy 23) $ filter (not.isDivisibleBy 19) $filter (not.isDivisibleBy 17) $filter (not.isDivisibleBy 11) ds

rules = map divisibleBy [2,3,5,7,13]  
filters = map (filter) rules
--nestedFilters = foldl (\filters f -> filters $ f) (id) filters



remainderMethod :: Int -> Int -> [Int] -> Bool
remainderMethod n multi ds
  | length ds <= 3 = (unDigs ds `mod` n == 0)
  | otherwise = divisibleBy n $ digs p where p = partial_result multi ds

partial_result times ds = let
  l = last ds
  remainder = unDigs $init ds
  in
  remainder + (times*l)




-----

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- | Takes a list of digits, and converts them back into a positive integer.
unDigs ::[Int] -> Int
unDigs = (foldl (\ a b -> a * 10 + b) 0)

-- | Takes a list of digits, and converts them back into a positive integer.
unDigits ::[Int] -> Integer
unDigits = toInteger.(foldl (\ a b -> a * 10 + b) 0)


------n
