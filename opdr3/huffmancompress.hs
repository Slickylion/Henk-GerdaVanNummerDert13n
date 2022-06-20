module Main where

import Data.Map as M
import Prelude as P
import Data.Maybe
import System.Environment
import Data.Function
import Data.List

type Mappy = M.Map Char Int

data HuufManTree = Leaf Char Int
    | Fork HuufManTree Int HuufManTree
    deriving (Show, Read)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg1, arg2, arg3] -> do
            text <- readFile arg1;
            let text2 = sortBy (compare `on` snd) (toList (countLetters text empty))
            print $ listToLeafList text2
            print text2
            putStrLn "YES WE GOT EM"
        _ -> putStrLn "NOT ENOUGH ARGUMENTS, give file to compress, file to save compressed, and file to save tree"

    -- De file waar de tree naartoe wordt geschreven

-- [('c',12),('b',10),('a',8),('d',3),('e',1)]

listToLeafList :: [(Char, Int)] -> [HuufManTree]
listToLeafList [] = []
listToLeafList ((x,y):rest) = Leaf x y : listToLeafList rest
--addToTree :: [[a]] -> HuufManTree
-- addToTree [] t = t
-- addToTree (x:rest) t = putStrLn x !! 0\

-- leafListToGeneralAppleTree :: [HuufManTree] -> HuufManTree
-- leafListToGeneralAppleTree 

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