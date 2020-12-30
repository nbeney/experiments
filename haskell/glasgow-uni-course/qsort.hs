lst :: [Int]
lst = [5, 1, 7, 10, 5, 4, 1, 5]

--qsort :: (Ord a) => [a] -> [a]
--qsort [] = []
--qsort (x:xs) = qsort left ++ [x] ++ qsort right
--  where
--    left = filter (<= x) xs
--    right = filter (> x) xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let
    left = filter (<= x) xs
    right = filter (> x) xs
  in
    qsort left ++ [x] ++ qsort right

main :: IO ()
main = do
  print $ res
  print $ length lst == length res
  where res = qsort lst
