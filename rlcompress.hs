-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)
module Main where
import System.IO  
import System.Directory
import System.Environment
import Data.List

runLength :: String -> String
runLength = concat . map (compress) . Data.List.group 

compress :: String -> String
compress x 
    | leng > 1 =(show . length $ x) ++ [head x]
    | otherwise = [head x]
    where leng = length $ x

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then putStrLn "please give file to read and file to write to"
        else do
         let command1 = head args
             command2 = args!! 1
         text <- readFile command1;
         let compressedText = runLength text
         let textLength = length text
         putStrLn("length of " ++ command1 ++ ": " ++ (show textLength) ++ " characters.")
         let compressedTextLength = length compressedText
         putStrLn("length of compressed file " ++ command2 ++ ": " ++ (show compressedTextLength) ++ " characters.")
         let compressionFactor = round $ ((fromIntegral compressedTextLength) / (fromIntegral textLength)) * 100
         putStrLn("factor: " ++ (show compressedTextLength) ++ " / " ++ (show textLength) ++ " * 100% = " ++ (show compressionFactor) ++ "%")
         writeFile command2 compressedText