module Main where

import Data.Map as M
import Prelude as P
import Data.Maybe

type Mappy = M.Map Char Int

main :: IO ()
main = do
    let maap = M.insert 'a' 9
    let x = M.lookup 'a'
    putStrLn "HEUFMAN"


countLetters :: String -> Mappy-> Mappy
countLetters [] m = m
countLetters (x:rest) m = countLetters (removeLetters rest x) (M.insert x (countLetter x rest+1) m)

removeLetters :: Eq a => [a] -> a -> [a]
removeLetters str char = P.filter (/= char) str

countLetter :: Char -> String -> Int
countLetter letter [] = 0
countLetter letter (x:rest)
    | letter == x = 1 + recursive
    | otherwise = recursive
    where recursive = countLetter letter rest