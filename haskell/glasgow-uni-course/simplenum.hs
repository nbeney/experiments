data SimpleNum = One | Two | Many deriving (Eq, Ord, Show, Read, Enum)

convert :: (Integral a) => a -> SimpleNum
convert n
  | n == 1    = One
  | n == 2    = Two
  | otherwise = Many

main :: IO ()
main = do
  print $ One
  print $ Two
  print $ Many
  print $ One == One
  print $ One /= Many
  print $ One < Many
  print $ map convert [0..5]
  print $ succ One
  print $ pred Many
