-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)
import Data.Char
import System.Environment
import Data.List


decompress :: String -> [String]
decompress [] = []
decompress (x:rest)
    | isDigit x = replicate (digitToInt x) (head rest) : decompress (tail rest)
    | otherwise = [x] : decompress rest

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