module Main where

main :: IO ()
main = do
    putStrLn "HEUFMAN"


countLetters :: String -> [String]
countLetters [] = []
countLetters (x:rest) = ["hou je mond compiler"]

countLetter :: Char -> String -> Int 
countLetter letter [] = 0
countLetter letter (x:rest)
    | letter == x = 1 + recursive
    | otherwise = recursive
    where recursive = countLetter letter rest