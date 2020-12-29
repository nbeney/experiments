-- TODO: Pick random word
-- TODO: Ignore blank inputs

possibleWords :: [String]
possibleWords = ["albania", "bulgaria", "croatia", "denmark", "england", "france", "germany"]

randomWord :: String
randomWord = possibleWords !! 0

initialDisplay :: String
initialDisplay = replicate (length randomWord) '-'

maxTries :: Int
maxTries = 5

turn :: String -> String -> Int -> IO ()
turn word display n
  | word == display = do
    putStrLn $ "Congrats, you won! The word was " ++ word ++ "."

  | n == 0 = do
    putStrLn $ "Sorry, you lost! The word was " ++ word ++ "."

  | otherwise = do
    mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do
    putStrLn $ display ++ "  " ++ (replicate n '*')
    putStrLn "Enter your guess: "
    q <- getLine
    let ch = if length q > 0 then q !! 0 else '@'
    let (correct, display') = check word display ch
    let n' = if correct then n else n - 1
    turn word display' n'

check :: String -> String -> Char -> (Bool, String)
check word display ch =
  (ch `elem` word, [if x == ch then ch else y | (x, y) <- zip word display])

main :: IO ()
main = do
  turn randomWord initialDisplay maxTries
