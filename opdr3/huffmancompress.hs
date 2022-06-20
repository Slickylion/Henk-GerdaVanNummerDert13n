module Main where

import Data.Map as M
import Prelude as P
import Data.Maybe
import System.Environment
import Data.Function
import Data.List

type Mappy = M.Map Char Int

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg1, arg2, arg3] -> do
            text <- readFile arg1;
            let text2 = sortBy (flip compare `on` snd) (toList (countLetters text empty))
            print text2
            putStrLn "YES WE GOT EM"
        _ -> putStrLn "NOT ENOUGH ARGUMENTS, give file to compress, file to save compressed, and file to save tree"

    -- De file waar de tree naartoe wordt geschreven



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