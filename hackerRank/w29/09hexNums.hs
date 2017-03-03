import Numeric
import Data.List
import Data.Maybe
import Data.Char


primeDigits = [2,3,5,7]
onlyPrimeDigits ds = all (\e->elem e primeDigits) ds

firstMegaNfrom :: Int -> Int
firstMegaNfrom n = unDigs $ fromJust $ find onlyPrimeDigits $map (digs) [n..]
  
digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

firstMegas n = take n $ map unDigs $filter onlyPrimeDigits $map (digs) [1..]
--associatons n = zip (firstMegas n) (map show_hex [0..n]) 
-- | Takes a list of digits, and converts them back into a positive integer.
unDigs ::[Int] -> Int
unDigs = (foldl (\ a b -> a * 10 + b) 0)


fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base).digitTo20Int) digitToInt
qf n = fromBase globalBase n

toBase :: Int -> Int -> String
toBase base num = showIntAtBase base intTo20Digit num ""


chars = "0123456789abcdefghij"
digitTo20Int :: Char -> Int
digitTo20Int c = fromJust $lookup c $zip chars [0..]:
intTo20Digit c = fromJust $lookup c $zip [0..] chars

qt n = toBase globalBase n

globalBase = 20

quat2Mega :: Int -> Int
quat2Mega c = let
  -- l = [(0,1),
  --      (1,2),
  --      (2,3),
  --      (3,5),
  --      (4,7)
  --      ]
  l = [(0,2),
       (1,3),
       (2,5),
       (3,7),
       
       (4,22),
       (5,23),
       (6,25),
       (7,27),
       
       (8,32),
       (9,33),
       (10,35),
       (11,37),
       
       (12,52),
       (13,53),
       (14,55),
       (15,57),
       
       (16,72),
       (17,73),
       (18,75),
       (19,77)
       ]

  in
    fromJust $lookup c l


fromBaseToBase :: Int -> Int -> String -> String
fromBaseToBase from to = toBase to . fromBase from

q2Mega num = quats2Mega 0 num
quats2Mega nest (-1)  = 0
--quats2Mega nest 0  = 0
quats2Mega nest num = let
  r = (num `mod` globalBase) -- `mod` 15
  d = (num `div` globalBase) 
  in
  ((quat2Mega r)*10^nest) + (quats2Mega (nest+1) (d-6)) 


            
