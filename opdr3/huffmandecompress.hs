module Main where

import Data.Map as M
import Prelude as P
import System.Environment
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Win32 (COORD(x))

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
            print snap
            print maptree
            print $ convertStringToHuufman text
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

convertStringToHuufman :: [Char] -> [[Char]]
convertStringToHuufman "" = []
convertStringToHuufman c = getUntilZero c : convertStringToHuufman (getAfterZero c)

getAfterZero :: [Char] -> [Char]
getAfterZero [] = []
getAfterZero (x:rest)
    | x == '0' = rest
    | otherwise = getAfterZero rest

getUntilZero :: [Char] -> [Char]
getUntilZero [] = []
getUntilZero (x:rest)
    | x == '0' = [x]
    | otherwise = x : getUntilZero rest

-- decodeShiiiietByHuufman :: [Char] -> Savvy -> [Char]
-- decodeShiiiietByHuufman [] s = []
-- decodeShiiiietByHuufman (x:rest) s = fromMaybe [x] M.lookup x (Map k a)

buildCodeMap :: HuufManTree -> [Char] -> Savvy-> Savvy
buildCodeMap (Leaf c i) b s = M.insert b c s
buildCodeMap (Fork x i y) b s = buildCodeMap x (b++"1") (buildCodeMap y (b++"0") s)
