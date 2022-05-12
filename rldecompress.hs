import Data.Char
import System.Environment

splitInput :: String -> [String]
splitInput [] = []
splitInput (x:y:rest) = if isDigit y
                         then let num1 = charToString x
                                  num2 = charToString y
                                  tot = num1 ++ num2
                                  c = head rest
                                  newRest = drop 1 rest
                               in [addToback tot c]++(splitInput newRest)
                         else [x, y]:(splitInput rest)
splitInput rest = [rest]--Prevent uneven list exception

charToString :: Char -> String
charToString c = [c]

addToback :: String -> Char -> String
addToback str c = str ++ [c]

decompress :: String -> String
decompress x = 
     let n = digitToInt $ head x
         c = last x
     in myReplicate n c

myReplicate :: Int -> a -> [a]
myReplicate 0 y = []
myReplicate x y = y : myReplicate (x-1) y

runLength :: String -> String
runLength x = concat . map (decompress) . splitInput $ x

main = do
-- Get the command line arguments and split them into two
         args <- getArgs
         let inputFile = head args
             outputFile = args!!1
-- Open input file and compress inputText into outputText
         inputText <- readFile inputFile;
         let outputText = runLength inputText
         writeFile outputFile outputText