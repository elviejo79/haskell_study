import Control.Applicative
import Control.Monad
import System.IO
-- import Data.Matrix
import Data.Array
import Data.List

main :: IO ()
main = do
    w_temp <- getLine
    let w_t = words w_temp
    let w = read $ w_t!!0 :: Int
    let h = read $ w_t!!1 :: Int
    circlex_temp <- getLine
    let circlex_t = words circlex_temp
    let circlex = read $ circlex_t!!0 :: Int
    let circley = read $ circlex_t!!1 :: Int
    let r = read $ circlex_t!!2 :: Int
    x1_temp <- getLine
    let x1_t = words x1_temp
    let x1 = read $ x1_t!!0 :: Int
    let y1 = read $ x1_t!!1 :: Int
    let x3 = read $ x1_t!!2 :: Int
    let y3 = read $ x1_t!!3 :: Int
    
    let thisC = isInsideCircle (circlex,(-circley)) r
    let thisD = isInsideDiamond (x1,(-y1)) (x3,(-y3))
    printFigure $ figureByFun (window w h) (figureOr thisC thisD)

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret



window :: Int->Int->Bounds
window w h =
  ((0,(h-1)*(-1)),((w-1),0))
                              
type Figure = Array Point Bool
type Bounds = (Point,Point)
type Point = (Int,Int)

figureOr :: (Point->Bool) -> (Point ->Bool) -> Point -> Bool
figureOr f1 f2 point = f1 point || f2 point



printFigure :: Figure -> IO ()
printFigure f = do
  putStrLn $ printArray $ f
  
topCorner :: Figure -> Figure -> Bounds
topCorner c d = let
  ((x1,y1),(x2,y2)) = bounds c
  ((x3,y3),(x4,y4)) = bounds d
  xs = [x1,x2,x3,x4]
  ys = [y1,y2,y3,y4]
  in
  ((minimum xs, minimum ys),(maximum xs, maximum ys))

printArray::Figure -> String
printArray arr = let
  showB :: Bool -> Char
  showB e = if e then '#' else '.'
  in
  unlines [[showB (arr ! (x, y)) | x <- [lowW..highW] ] | y <- [highH,highH-1..lowH]]
  where ((lowW, lowH), (highW, highH)) =  bounds arr

isInsideCircle :: (Int,Int) -> Int ->(Int,Int) -> Bool
isInsideCircle (centerX,centerY) r (x,y) = ((centerX-x)^2+(centerY-y)^2 <= r^2)


  



circle :: Point ->Int -> Figure
circle (centerX,centerY) r = let
  (topX,topY) = (centerX-r, centerY+r)
  (bottomX,bottomY)=(centerX+r, centerY-r)
  in
    figureByFun (enclosingBounds [(topX,topY),(bottomX,bottomY)]) (isInsideCircle (centerX,centerY) r) 


  
diamond :: Point -> Point -> Figure
diamond p1 p3 = let
  (_,p2,_,p4) = findRhombus p1 p3
  in
    figureByFun (enclosingBounds [p1,p2,p3,p4]) (isInsideDiamond p1 p3)

isInsideDiamond :: Point -> Point -> Point -> Bool
isInsideDiamond a d m = let
  (_,b,_,c) = findRhombus a d
  am = vect a m
  ab = vect a b
  ac = vect a c
  am_dot_ab = am `dot` ab
  am_dot_ac = am `dot` ac
  in
  (0 <= am_dot_ab && am_dot_ab <= (ab `dot` ab)) &&
  (0 <= am_dot_ac && am_dot_ac <= (ac `dot` ac))

vect::Point -> Point -> Point
vect (x1,y1) (x2,y2) = (x2-x1,y2-y1)

dot::Point -> Point -> Int
dot (x1,y1) (x2,y2) = x1*x2+y1*y2


dist2 :: Point->Point->Int
dist2 (x1,y1) (x2,y2) = (x2-x1)^2 + (y2-y1)^2

figureByFun :: Bounds -> (Point->Bool) -> Figure
figureByFun ((topX, topY),(bottomX,bottomY)) isInsideFigure =
  array ((topX,topY), (bottomX, bottomY)) [((i, j), isInsideFigure (i,j)) | i <- [topX..bottomX], j <- [topY..bottomY]]

enclosingBounds :: [Point] -> Bounds
enclosingBounds points = let
  separateXsYs :: ([Int],[Int]) -> Point -> ([Int],[Int])
  separateXsYs (xxs,yys) (x,y) = (x:xxs,y:yys)
  (xs,ys) = foldl separateXsYs ([],[]) points
  in
  ((minimum xs, minimum ys),(maximum xs, maximum ys))
  --((0,(-30)),(16,0))

enclosingRec :: [Bounds] -> Bounds
enclosingRec bs = enclosingBounds $ foldl (\acum (p1,p2)->acum ++ [p1,p2]) [] bs

findRhombus :: Point->Point->(Point,Point,Point,Point)
findRhombus (x1,y1) (x2,y2) = let
  p3 = (((x1+x2+y2-y1) `div` 2),((y1+y2+x1-x2) `div` 2))
  p4 = (((x1+x2+y1-y2) `div` 2),((y1+y2+x2-x1) `div` 2))
  in
  ((x1,y1),p4, (x2,y2),p3)
