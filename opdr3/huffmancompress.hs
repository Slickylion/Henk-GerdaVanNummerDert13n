module Main where

import Data.Map as M
import Prelude as P
import System.Environment
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)

type Mappy = M.Map Char Int
type Snappy = M.Map Char [Char]

data HuufManTree = Leaf Char Int
    | Fork HuufManTree Int HuufManTree
    deriving (Show, Read)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg1, arg2, arg3] -> do
            text <- readFile arg1
            let text2 = sortBy (compare `on` snd) (toList (countLetters text empty))
            let tree = leafListToGeneralAppleTree $ listToLeafList text2

            let codeMap = buildCodeMap tree "" empty
            let textEncoded = encodeShiiiietByHuufman text codeMap
            let encodedBits = length textEncoded
            let bits = 8 * length text

            putStrLn ("Length of " ++ arg1 ++": " ++ show (length text) ++ " characters, " ++ show bits ++ " bits.")
            putStrLn ("length of compressed file " ++ arg2 ++": " ++ show encodedBits ++ " bits.")
            putStrLn ("factor: " ++ show encodedBits ++ "/" ++ show bits ++ "*100=" ++ show(round $ fromIntegral (length textEncoded) / fromIntegral bits * 100) ++ "%")

            writeFile arg3 $ show tree
            putStrLn ("file " ++ arg2 ++ " written to disk...")
            writeFile arg2 textEncoded
            putStrLn ("file " ++ arg3 ++ " written to disk...")
            putStrLn "done..."
            
        _ -> putStrLn "NOT ENOUGH ARGUMENTS, give file to compress, file to save compressed, and file to save tree"

    -- De file waar de tree naartoe wordt geschreven

-- [('c',12),('b',10),('a',8),('d',3),('e',1)]
listToLeafList :: [(Char, Int)] -> [HuufManTree]
listToLeafList [] = []
listToLeafList ((x,y):rest) = Leaf x y : listToLeafList rest

buildCodeMap :: HuufManTree -> [Char] -> Snappy-> Snappy
buildCodeMap (Leaf c i) b s = M.insert c b s
buildCodeMap (Fork x i y) b s = buildCodeMap x (b++"1") (buildCodeMap y (b++"0") s)

encodeShiiiietByHuufman :: [Char] -> Snappy -> [Char]
encodeShiiiietByHuufman rest snap
  = P.foldr (\ x -> (++) (fromMaybe [x] (M.lookup x snap))) [] rest

getValue :: HuufManTree -> Int
getValue (Leaf _ i) = i
getValue (Fork _ i _) = i

addToTree :: HuufManTree -> HuufManTree -> HuufManTree
addToTree l t = Fork l (getValue l + getValue t) t

leafListToGeneralAppleTree :: [HuufManTree] -> HuufManTree
leafListToGeneralAppleTree c = leafListToGeneralAppleTreeRecursive (tail c) (head c)

leafListToGeneralAppleTreeRecursive  :: [HuufManTree] -> HuufManTree -> HuufManTree
leafListToGeneralAppleTreeRecursive rest t = P.foldl addToTree t rest

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