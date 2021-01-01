import Control.Monad
import Data.Char
import System.IO

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter isShort . lines
  where isShort line = length line < 10

-- V1 = forever + getLine

test1 :: IO ()
test1 = forever $ do
  line <- getLine
  print $ unwords . map reverse . words . map toUpper $ line

-- V2 - getContents

test2 :: IO ()
test2 = do
  contents <- getContents
  putStr $ map toUpper contents

-- V3 - getContents

test3 :: IO ()
test3 = do
  contents <- getContents
  putStr $ shortLinesOnly contents

-- V4 - interact

test4 :: IO ()
test4 = interact shortLinesOnly

-- V5 - openFile

test5 :: IO ()
test5 = do
  handle <- openFile "capslocker.txt" ReadMode
  contents <- hGetContents handle
  putStr $ contents
  hClose handle

-- V6 - withFile

test6 :: IO ()
test6 = do
  n <- withFile "capslocker.txt" ReadMode process6
  putStrLn $ show n ++ " lines"

process6 :: Handle -> IO Int
process6 handle = do
  contents <- hGetContents handle
  putStr $ contents
  return $ length . lines $ contents

-- V6 - readFile

test7 :: IO ()
test7 = do
  contents <- readFile "capslocker.txt"
  putStr $ contents

-- main

main :: IO ()
main = test7
