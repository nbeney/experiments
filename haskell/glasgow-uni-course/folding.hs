foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs 

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "Invalid argument!"
foldl1' f (x:xs) = foldl' f x xs

main :: IO ()
main = do
    print $ foldl' (+) 5 []
    print $ foldl' (+) 5 [1..10]
    print $ foldl1' (+) [1..10]
