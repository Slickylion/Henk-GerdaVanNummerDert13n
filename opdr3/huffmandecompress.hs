module Main where

import Data.Map as M
import Prelude as P
import System.Environment
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)

type Mappy = M.Map Char Int
type Savvy = M.Map [Char] Char

data HuufManTree = Leaf Char Int
    | Fork HuufManTree Int HuufManTree
    deriving (Show, Read)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg1, arg2, arg3] -> do
            text <- readFile arg1
            boomtext <- readFile arg3
            let maptree = read boomtext::HuufManTree
            let snap = buildCodeMap maptree "" empty
            print maptree
            print snap
            print $ convertStringToHuufman text $ getMaxStringLengthFromMap snap
            putStrLn $ decodeShiiiietByHuufman (convertStringToHuufman text $ getMaxStringLengthFromMap snap) snap

            -- let text2 = sortBy (compare `on` snd) (toList (countLetters text empty))
            -- let tree = leafListToGeneralAppleTree $ listToLeafList text2

            -- let codeMap = buildCodeMap tree "" empty
            -- let textEncoded = encodeShiiiietByHuufman text codeMap
            -- let encodedBits = length textEncoded
            -- let bits = 8 * length text

            -- putStrLn ("Length of " ++ arg1 ++": " ++ show (length text) ++ " characters, " ++ show bits ++ " bits.")
            -- putStrLn ("length of compressed file " ++ arg2 ++": " ++ show encodedBits ++ " bits.")
            -- putStrLn ("factor: " ++ show encodedBits ++ "/" ++ show bits ++ "*100=" ++ show(round $ fromIntegral (length textEncoded) / fromIntegral bits * 100) ++ "%")

            -- writeFile arg3 $ show tree
            -- putStrLn ("file " ++ arg2 ++ " written to disk...")
            -- writeFile arg2 textEncoded
            -- putStrLn ("file " ++ arg3 ++ " written to disk...")
            -- putStrLn "done..."

        _ -> putStrLn "NOT ENOUGH ARGUMENTS, give file to decompress, file to save decompressed, and file with tree"

convertStringToHuufman :: [Char] -> Int -> [[Char]]
convertStringToHuufman "" i = []
convertStringToHuufman c i = getUntilZero c 1 i: convertStringToHuufman (getAfterZero c 1 i) i

getMaxStringLengthFromMap:: Savvy -> Int
getMaxStringLengthFromMap s = M.size s - 1

decodeShiiiietByHuufman :: [[Char]] -> Savvy -> [Char]
decodeShiiiietByHuufman rest s
  = P.foldr (\ x -> (++) (maybeCharToString $ M.lookup x s)) [] rest


maybeCharToString :: Maybe Char -> [Char]
maybeCharToString c = [fromMaybe ' ' c]

getAfterZero :: [Char] -> Int -> Int -> [Char]
getAfterZero [] i m= []
getAfterZero (x:rest) i m
    | x == '0' = rest
    | i == m = rest
    | otherwise = getAfterZero rest (i+1) m

getUntilZero :: [Char] -> Int -> Int -> [Char]
getUntilZero [] i m= []
getUntilZero (x:rest) i m
    | x == '0' = [x]
    | i == m = [x]
    | otherwise = x : getUntilZero rest (i+1) m
buildCodeMap :: HuufManTree -> [Char] -> Savvy-> Savvy
buildCodeMap (Leaf c i) b s = M.insert b c s
buildCodeMap (Fork x i y) b s = buildCodeMap x (b++"1") (buildCodeMap y (b++"0") s)
