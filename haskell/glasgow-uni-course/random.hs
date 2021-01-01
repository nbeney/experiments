import System.Random

data Color = A | B | C | D | E | F | G | H | I | J | K | L deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Random Color where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random g = randomR (minBound, maxBound) g
  
main :: IO ()
main = do
  g <- getStdGen
  print $ take 10 (randomRs ('a', 'z') g)
  print $ take 10 (randomRs (1 :: Int, 100) g)
  print $ take 10 (randoms g :: [Color])
  print $ take 10 (randomRs (A, F) g)

  
