import Data.Char
import System.Environment
import Data.List


splitRunLength :: String -> [String]
splitRunLength [] = []
splitRunLength (x:rest)
    | isDigit x = replicate (digitToInt x) (head rest) : end
    | otherwise = [x] : splitRunLength rest
        where end = splitRunLength (tail rest)

main :: IO ()
main = do
-- Get the command line arguments and split them into two
         args <- getArgs
         let inputFile = head args
             outputFile = args!!1
-- Open input file and compress inputText into outputText
         inputText <- readFile inputFile;
         let outputText = splitRunLength inputText
         writeFile outputFile $ concat outputText