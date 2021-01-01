solveRPN :: String -> [Float]  
solveRPN = foldl fn [] . words  
  where
    fn (y:x:ys) "*" = (x * y):ys  
    fn (y:x:ys) "+" = (x + y):ys  
    fn (y:x:ys) "-" = (x - y):ys  
    fn (y:x:ys) "/" = (x / y):ys  
    fn (y:x:ys) "^" = (x ** y):ys
    fn (y:x:ys) ".." = reverse [x..y] ++ ys

    fn (x:xs) "!" = product [1..x]:xs
    fn (x:xs) "abs" = abs x:xs  
    fn (x:xs) "neg" = negate x:xs  
    fn (x:xs) "inv" = (1.0 / x):xs  
    fn (x:xs) "sqrt" = sqrt x:xs  
    fn (x:xs) "ln" = log x:xs  
    fn (x:xs) "cos" = cos x:xs  
    fn (x:xs) "sin" = sin x:xs  
    fn (x:xs) "tan" = tan x:xs  

    fn xs "e" = exp 1:xs  
    fn xs "pi" = pi:xs
    
    fn xs "avg" = [sum xs / fromIntegral (length xs)]
    fn xs "cnt" = [fromIntegral (length xs)]
    fn xs "max" = [maximum xs]
    fn xs "min" = [minimum xs]
    fn xs "prd" = [product xs]
    fn xs "sum" = [sum xs]
    
    fn xs numberString = read numberString:xs

test :: String -> IO ()
test expr = putStrLn $ expr ++ " => " ++ show (solveRPN expr)
  
main :: IO ()
main = do
  test "1"
  test "-2.5"
  test "1 2 +"
  test "1 2 + 3 *"
  test "1 2 3 + *"
  test "1 2 3 4 5"
  test "1 5 .."
  test "e"
  test "pi"
  test "1 2 + 3 * 1 -"
  test "1 2 + 3 * 1"
  test "2 pi * 2 2 ^ *"
  test "pi 8 *"
  test "-4 abs"
  test "4 neg"
  test "4 inv"
  test "16 sqrt"
  test "e ln"
  test "pi cos"
  test "pi 2 / sin"
  test "pi 2 / tan"
  test "5 !"

  test "1 5 .. avg"
  test "1 5 .. cnt"
  test "1 5 .. max"
  test "1 5 .. min"
  test "1 5 .. prd"
  test "1 5 .. sum"
  
