import Data.Char
import System.Environment
import Data.List


decompress :: String -> [String]
decompress [] = []
decompress (x:rest)
    | isDigit x = replicate (digitToInt x) (head rest) : end
    | otherwise = [x] : decompress rest
        where end = decompress (tail rest)

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