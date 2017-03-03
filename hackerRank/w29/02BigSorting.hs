import Control.Applicative
import Control.Monad
import System.IO
import Data.List

main :: IO ()
main = do
  n_temp <- getLine
  let n = read n_temp :: Int
  unsorted <- getMultipleLines n
  let intUnsorted = map (read::String->Integer) unsorted
  let sorted = sort intUnsorted
  
  putStr $ unlines $ map (show) sorted


getMultipleLines :: Int -> IO [String]
getMultipleLines n
  | n <= 0 = return []
  | otherwise = do
      x <- getLine
      xs <- getMultipleLines (n-1)
      let ret = (x:xs)
      return ret
                                                                                                   
