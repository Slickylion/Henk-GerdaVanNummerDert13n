-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)
import Data.Char
import System.Environment
import Data.List

decompress :: String -> [String]
decompress [] = []
decompress (x:rest)
    | isDigit x = replicate (read (x:takeDigits rest)::Int) (head left) : decompress (tail left)
    | otherwise = [x] : decompress rest
    where left = removeDigits rest

takeDigits :: String -> String
takeDigits [] = []
takeDigits (x:rest)
    | isDigit x = x : takeDigits rest
    | otherwise = ""
    
removeDigits :: String -> String
removeDigits [] = []
removeDigits (x:rest)
    | isDigit x = removeDigits rest
    | otherwise = x:rest

main :: IO ()
main = do
-- Get the command line arguments and split them into two
         args <- getArgs
         let inputFile = head args
             outputFile = args!!1
-- Open input file and compress inputText into outputText
         inputText <- readFile inputFile;
         let outputText = decompress inputText
         writeFile outputFile $ concat outputText