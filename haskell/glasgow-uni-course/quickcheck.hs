import Data.Char
import Test.QuickCheck

shouldcipher :: Char -> Bool
shouldcipher c = isLetter c && isAscii c

cipherchar :: Int -> Char -> Char
cipherchar shift c
  | shouldcipher c = roll c (chr $ ord c + shift')
  | otherwise = c
  where shift' = shift `mod` 26

cipher :: Int -> String -> String
cipher shift s = map (cipherchar shift) s
  
decipher :: Int -> String -> String
decipher shift s = cipher (-shift) s

roll :: Char -> Char -> Char
roll o c
  | isUpper o && c < 'A' = chr $ ord c + 26
  | isUpper o && c > 'Z' = chr $ ord c - 26
  | isLower o && c < 'a' = chr $ ord c + 26
  | isLower o && c > 'z' = chr $ ord c - 26
  | otherwise = c

test = do
  let prop n s = (decipher n (cipher n s)) == s
  verboseCheck prop
  quickCheck prop
    
main :: IO ()
main = do
  print $ source
  print $ crypto
  print $ target

  test

  where shift = 3
        source = "Hello world!"
        crypto = cipher shift source
        target = decipher shift crypto


