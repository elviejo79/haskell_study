import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "QuickCheck Tutorial" [
                testProperty "reverse of reverse" prop_RevRev,
                testProperty "given a condition the next must be true" prop_insert1,
                testProperty "with forAll generate data" prop_insert2,
                testProperty "classifiying test cases" prop_insert_classify,
                testProperty "collecting data" prop_insert_collect
           ]
      ]

prop1 b = b == (not $ not $ b)
  where types = (b :: Bool)

prop2 i = i == 42
  where types = (i :: Int)

prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

-- Given the first condition then the second must be true
prop_insert1 x xs = ordered xs ==> ordered (insert x xs)
  where types= x::Int

-- The first argument of forAll is a test data generator;
-- by supplying a custom generator, it is possible to control the distribution of test data.
prop_insert2 x = forAll orderedList (\xs-> ordered (insert x xs))
  where types = x::Int

-- Counting Trivial Cases
-- Test case for which the condition is True ar classified as trivial and reported
-- prop_insert_trivial x xs = ordered xs ==> null xs `trivial` ordered (insert x xs)
--   where types x = x::Int

-- Classifying Test Cases
-- test satisfying the condition are assigned to the given classification and reported
prop_insert_classify x xs =
    ordered xs ==>
      classify (ordered (x:xs)) "at-head" $
      classify (ordered (xs++[x])) "at-tail" $
      ordered (insert x xs)
  where types = x::Int
    

-- Collecting Data Values
-- The argument of collect is evaluated in each test case, and the distribution of values is reported
prop_insert_collect x xs =
  ordered xs ==> collect (length xs)$
                 ordered (insert x xs)
  where types = x::Int
