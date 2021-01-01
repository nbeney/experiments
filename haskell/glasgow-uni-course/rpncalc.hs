evaluateRPN :: String -> [Float]  
evaluateRPN = foldl eval [] . words  
  where
    eval xs "e" = exp 1:xs  
    eval xs "pi" = pi:xs
    
    eval (x:xs) "!" = product [1..x]:xs
    eval (x:xs) "abs" = abs x:xs  
    eval (x:xs) "neg" = negate x:xs  
    eval (x:xs) "inv" = (1.0 / x):xs  
    eval (x:xs) "sqrt" = sqrt x:xs  
    eval (x:xs) "ln" = log x:xs  
    eval (x:xs) "cos" = cos x:xs  
    eval (x:xs) "sin" = sin x:xs  
    eval (x:xs) "tan" = tan x:xs  

    eval (y:x:ys) "*" = (x * y):ys  
    eval (y:x:ys) "+" = (x + y):ys  
    eval (y:x:ys) "-" = (x - y):ys  
    eval (y:x:ys) "/" = (x / y):ys  
    eval (y:x:ys) "^" = (x ** y):ys
    eval (y:x:ys) ".." = reverse [x..y] ++ ys
    
    eval xs "avg" = [sum xs / fromIntegral (length xs)]
    eval xs "cnt" = [fromIntegral (length xs)]
    eval xs "max" = [maximum xs]
    eval xs "min" = [minimum xs]
    eval xs "prd" = [product xs]
    eval xs "sum" = [sum xs]

    eval xs numberString = read numberString:xs

test :: String -> IO ()
test expr = putStrLn $ expr ++ " => " ++ show (evaluateRPN expr)
  
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
  
