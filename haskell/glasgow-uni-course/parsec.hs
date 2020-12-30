import Text.ParserCombinators.Parsec (parseTest, char)

main :: IO ()
main = do
  print $ fmap id a
  print $ fmap id b
  where a = parseTest (char 'c') "cons"
        b = parseTest (char 'b') "boss" 
