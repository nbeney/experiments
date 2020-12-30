maxElement :: Ord a => [a] -> Maybe a
maxElement [] = Nothing
maxElement (x:xs) = Just (go x xs)
  where go m [] = m
        go m (y:ys) = go (max m y) ys

inc :: Num a => a -> a
inc = (+1)

main :: IO ()
main = do
  print $ maxElement ([] :: [Int])
  print $ maxElement ([1, 2, 3] :: [Int])

  print $ "fmap Nothing = " ++ show (fmap inc Nothing)
  print $ "fmap Just 4  = " ++ show (fmap inc (Just (4 :: Int)))
  
  print $ "fmap [1..5]  = " ++ show (fmap inc [1..5] :: [Int])
