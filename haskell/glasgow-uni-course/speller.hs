import Data.List (intercalate)

spell :: String -> String
spell word = head word : " is for " ++ word

speller :: [String] -> String
speller words = intercalate ", " $ map spell words 

main :: IO ()
main = do
    print $ spell "alpha"
    print $ speller ["alpha", "bravo", "charlie", "delta", "fox", "golf"]
    print $ speller []
    
