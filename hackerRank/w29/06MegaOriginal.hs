import Control.Monad
import System.Random
import System.IO.Unsafe

candidatesN :: Int -> [Int]
candidatesN m = map unDigits $candidates m


-- | Takes a list of digits, and converts them back into a positive integer.
unDigits :: Integral n
    => [n] -- ^ The digits of the number in list form.
    -> n   -- ^ The original number.
unDigits = foldl (\ a b -> a * 10 + b) 0
                         
candidates :: Int -> [[Int]]
candidates magnitude =
  let
    previousMagnitude = magnitude -1
    appendDigits :: [Int]->[[Int]]
    appendDigits ns = [ns++[7],ns++[3]]
  in
    filter combined_condition $ replicateM previousMagnitude [2,3,5,7]

isDivisbleBy :: Int -> [Int]->[Int] ->Bool
isDivisbleBy n sequence ns = ((sum $ zipWith (*) ns (cycle sequence)) `mod` n == 0)

isDivisbleBy3 ns = isDivisbleBy 3 [1] ns

isDivisbleBy7 :: [Int]->Bool
isDivisbleBy7 ns = isDivisbleBy 7 [1,3,2,6,4,5] (reverse ns)

isDivisbleBy11 ns = (even $ length ns) && (and $ zipWith (==) ns (tail ns))
isDivisbleBy13 ns = isDivisbleBy 13 [1,10,9,12,3,4] (reverse ns)

combined_condition :: [Int] -> Bool
combined_condition ns = let
  conditions = [
    (even.last),
    (isDivisbleBy 3 [1]),
    ((isDivisbleBy 7 [1,3,2,6,4,5]).(reverse)),
    (isDivisbleBy11),
    ((isDivisbleBy 13 [1,10,9,12,3,4]).(reverse))
               ]
  in
    not $ or $ map ($ ns) conditions

-----------------------------
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
  | n < 3825123056546413051 = return [2, 3, 5, 7, 11, 13, 17, 19, 23]
  | otherwise = return []
  -- | otherwise           = do g <- newStdGen
  --                                 return $ take k (randomRs (2,n-1) g)

primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- powerMod m x n = x^n `mod` m
powerMod :: Integral nat => nat -> nat -> nat -> nat
powerMod m x n  = f (n - 1) x x `rem` m
  where
    f d a y = if d==0 then y else g d a y
    g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
            | otherwise = f (i-1) b (b*y `rem` m)


choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k
combinations_formula n r = 
