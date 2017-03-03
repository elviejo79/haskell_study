import Control.Applicative
import Control.Monad
import System.IO



main :: IO ()
main = do
    y_temp <- getLine
    let y = read y_temp :: Int
    putStrLn (dayOfProgrammer y)             -- your code goes here
    

dayOfProgrammer :: Int -> String
dayOfProgrammer year 
  | year == 1918 = "27.09."++show year
  | isLeap year = "12.09." ++ show year
  | otherwise = "13.09." ++ show year

isLeap :: Int -> Bool
isLeap year
  | year < 1918 = isJulian year
  | year > 1918 = isGregorian year
  | year == 1918 = False
  where
    isJulian year = mod year 4 == 0
    isGregorian year
        | mod year 400 == 0 = True
        | mod year 100 == 0 = False
        | mod year 4 == 0 = True
        | otherwise = False
  
  
getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
                                        
