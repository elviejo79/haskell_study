import Numeric
import Data.Maybe
import Data.Char
import Data.List

import System.Random
import System.IO.Unsafe

fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base).digitToInt) digitToInt

toBase :: Int -> Int -> String
toBase base num = showIntAtBase base intToDigit num ""

fromBaseToBase :: Int -> Int -> String -> String
fromBaseToBase from to = toBase to . fromBase from

-- megaToBase5 :: String -> String
-- megaToBase5 strN = let
--   charRemap c = fromJust $lookup c $zip "02357" "01234"
--   in
--   map charRemap strN

-- base5ToMega :: String -> String
-- base5ToMega strN = let
--   charRemap c = fromJust $lookup c $zip "01234" "02357"
--   in
--   map charRemap strN
readMega = (read::String -> Integer)
megaNums = "02357"
(megaToBase5,base5ToMega) = coderDecoder megaNums "01234"

megaAbove :: Integer -> Integer
megaAbove n = let
  searchStart = findBaseMegaSeq n
  in
  fromJust $ find (>n) $map (readMega.intToMega) [searchStart..]

isIntMega n = all (\x -> x `elem` megaNums ) $ toBase 10 n
  
intToMega :: Integer ->String
intToMega n = base5ToMega $ intToBase5 n

megaToInt :: String -> Int
megaToInt n = base5ToInt $ megaToBase5 n

intToBase5 :: Integer -> String
intToBase5 n = decToBase5 $ show n
base5ToInt :: String -> Int
base5ToInt b5 = (read::String->Int)  $ base5ToDec b5

decToBase5 :: String -> String
decToBase5 strDec = fromBaseToBase 10 5 strDec
base5ToDec :: String -> String
base5ToDec strBase5 = fromBaseToBase 5 10 strBase5

coderDecoder :: String -> String -> (String->String,String->String)
coderDecoder origen destino = ((coder origen destino),(coder destino origen))
coder ::String -> String -> String -> String
coder orginal destino strN = let
  charRemap c = fromJust $lookup c $zip orginal destino
  in
  map charRemap strN
  
findBaseMegaSeq :: Integer -> Integer
findBaseMegaSeq n = let
  -- In order to generate this numbers used to short the searrch use this code
  -- zip (map readMega sevens) $map megaToInt $"0":sevens
  -- let sevens = map (\x -> take x (repeat '7')) [1..17]
  
  l= [(7,0),(77,4),(777,24),(7777,124),(77777,624),(777777,3124),(7777777,15624),(77777777,78124),(777777777,390624),(7777777777,1953124),(77777777777,9765624),(777777777777,48828124),(7777777777777,244140624),(77777777777777,1220703124),(777777777777777,6103515624),(7777777777777777,30517578124),(77777777777777777,152587890624)]
  
  resultTuple = fromJust $ find (\(k,_) -> n <(k-1)) l
  in
  snd resultTuple
  
  
-- ==================================================================
-- Taken from:https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Haskell
--module Primes where


  -- Miller-Rabin wrapped up as an (almost deterministic) pure function
isPrime :: Integer -> Bool
isPrime n = unsafePerformIO (isMillerRabinPrime 100 n)


isMillerRabinPrime :: Int -> Integer -> IO Bool
isMillerRabinPrime k n
  | even n    = return (n==2)
  | n < 100   = return (n `elem` primesTo100)
  | otherwise = do ws <- witnesses k n
                   return $ and [test n (pred n) evens (head odds) a | a <- ws]
  where
    (evens,odds) = span even (iterate (`div` 2) (pred n))

test :: Integral nat => nat -> nat -> [nat] -> nat -> nat -> Bool
test n n_1 evens d a = x `elem` [1,n_1] || n_1 `elem` powers
  where
    x = powerMod n a d
    powers = map (powerMod n a) evens

witnesses :: (Num a, Ord a, Random a) => Int -> a -> IO [a]
witnesses k n
  | n < 9080191         = return [31,73]
  | n < 4759123141      = return [2,7,61]
  | n < 3474749660383   = return [2,3,5,7,11,13]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | n < 3825123056546413051 = return [ 2, 3, 5, 7, 11, 13, 17, 19,23]
  | n < 18446744073709551616 = return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
  | otherwise           = do g <- newStdGen
                             return $ take k (randomRs (2,n-1) g)

primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- powerMod m x n = x^n `mod` m
powerMod :: Integral nat => nat -> nat -> nat -> nat
powerMod m x n  = f (n - 1) x x `rem` m
  where
    f d a y = if d==0 then y else g d a y
    g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
            | otherwise = f (i-1) b (b*y `rem` m)


seq2Megas= take 1000 $ zip [1..] $map (readMega.intToMega) [1..]

lastResort = let
  largest = fromIntegral $ megaToInt "7777777777777777"
  smallest = fromIntegral $ megaToInt "2222222"
  in
  map (readMega.intToMega)  ([largest,largest-1..smallest]::[Integer])

lastLastResort = let
  byDigits = (map (isDivisibleBy) (reverse [2,3,5,7,11,13,17,19,23]))

 in
  (chainFilters byDigits  lastResort)
  
  
chainFilters :: [(a -> Bool)]->[a]->[a]
chainFilters [] ds = ds
chainFilters fs ds = filter (head fs) $ chainFilters (tail fs) ds

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy 2 numerator = odd numerator
isDivisibleBy denominator numerator = 0 /= flip mod denominator numerator

-- taken from http://stackoverflow.com/a/3963628/54848
digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)
