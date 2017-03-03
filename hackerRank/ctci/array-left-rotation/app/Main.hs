module Main where

import Lib

main :: IO ()
main = do
  n_temp <- getLine
  let n_t = words n_temp
  let n = read $ n_t!!0::Int
  let k = read $ n_t!!1::Int
  a_temp <- getLine
  let a = map read $ words a_temp::[Int]
  
  print $ shiftLeft k a
shiftLeft:: Int->[Int]->[Int]
shiftLeft d xs = let
  (begin,end) = splitAt 2 xs
  in
  end++begin
