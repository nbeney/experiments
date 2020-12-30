data Point = Point {
  x :: Double,
  y :: Double
} deriving (Eq, Show, Read)
             
data Shape =
    Circle {center :: Point, radius :: Double}
  | Rectangle {topLeft :: Point, bottomRight :: Point}
  | Line {start :: Point, end :: Point}
  deriving (Eq, Show, Read)

area :: Shape -> Double
area (Circle _ r) = pi * r ^ 2
area r@(Rectangle _ _) = (width r) * (height r)
area (Line _ _) = 0

perimeter :: Shape -> Double
perimeter (Circle _ r) = 2 * pi * r
perimeter r@(Rectangle _ _) = 2 * (width r) +  2 * (height r)
perimeter (Line start end) = distance start end

width :: Shape -> Double
width (Circle _ r) = 2 * r
width (Rectangle (Point x1 _)  (Point x2 _)) = x2 - x1
width (Line _ _ ) = error "Not applicable!"

height :: Shape -> Double
height (Circle _ r) = 2 * r
height (Rectangle (Point _ y1)  (Point _ y2)) = y2 - y1
height (Line _ _ ) = error "Not applicable!"

isSquare :: Shape -> Bool
isSquare (Circle _ _) = False
isSquare r@(Rectangle _ _) = (width r) == (height r)
isSquare (Line _ _) = False

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt (dx ^ 2 + dy ^ 2)
  where dx = x2 - x1
        dy = y2 - y1

main :: IO ()
main = do
  let c = Circle (Point 100 200) 50
  let r = Rectangle (Point 100 200) (Point 500 400)
  let s = Rectangle (Point 100 200) (Point 400 500)
  let l = Line (Point 100 200) (Point 400 500)
  print $ "c = " ++ show c ++ ", area = " ++ show (area c) ++ ", perimeter = " ++ show (perimeter c) ++ ", square? = " ++ show (isSquare c)
  print $ "r = " ++ show r ++ ", area = " ++ show (area r) ++ ", perimeter = " ++ show (perimeter r) ++ ", square? = " ++ show (isSquare r)
  print $ "s = " ++ show r ++ ", area = " ++ show (area s) ++ ", perimeter = " ++ show (perimeter s) ++ ", square? = " ++ show (isSquare s)
  print $ "l = " ++ show l ++ ", area = " ++ show (area l) ++ ", perimeter = " ++ show (perimeter l) ++ ", square? = " ++ show (isSquare l)
