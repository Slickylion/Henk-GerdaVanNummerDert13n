import System.IO  
import System.Directory
import System.Environment
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as DM

type HC = [(Char, Int)]

stringToList :: String -> HC
stringToList = DM.toList . DM.fromListWith (+) . map (flip (,) 1)
-- Using the 'flip' function with the ',' makes a tuple of the pair
-- Applying map on that makes that happen with every char in the string, so you get a list of tuples
-- Using Data.Map's fromListWith function you can add the duplicate tuples and increse the num inside
-- Problem is, the result isn't a real list, it's a map. toList makes it a HC list

sortList :: HC -> HC
sortList = sortBy (compare `on` snd)

makeCodeTable :: HC -> String -> HC
makeCodeTable (x:xs) str = if str == []
                             then let c = fst x
                                      code = "0"
                                 in [(c, stringToInt code)]++(makeCodeTable xs code)
                             else if xs /= []
                                     then let c = fst x
                                              code = '1':str
                                         in [(c, stringToInt code)]++(makeCodeTable xs code)
                                     else let c = fst x
                                              code = addToback (init str) '1'
                                         in [(c, stringToInt code)]
-- TO DO: Fix this monster

addToback :: String -> Char -> String
addToback str c = str ++ [c]

stringToInt :: String -> Int
stringToInt = read

encode :: HC -> String -> String
encode t str = let m = DM.fromList t
                   intList = map (m DM.!) str
                 in concat $ map (show) intList
-- Give codeTable and input string
-- Receive encoded string

tupleToPrintableString :: (Char, Int) -> String
tupleToPrintableString (c, n) = (c:(show n)) ++ ","

main = do
-- Get the command line arguments and split them into two
         args <- getArgs
         let inputFile = head args
             outputFile = args!!1
             treeFile = args!!2
-- Open input file and compress inputText into outputText
         inputText <- readFile inputFile;
         let codeTable = makeCodeTable (sortList $ stringToList inputText) ""
             treeTable = init $ concat $ map (tupleToPrintableString) codeTable
             outputText = encode codeTable inputText
-- Calulate and print length of inputFile
         let inputLenghtInBits = (length inputText) * 8
             inputLength = length inputText
         putStrLn("length of " ++ inputFile ++ ": " ++ (show inputLength) ++ " characters, " ++ (show inputLenghtInBits) ++ " bits.")
-- Calulate and print length of outputFile
         let outputLength = length outputText
         putStrLn("length of compressed file " ++ outputFile ++ ": " ++ (show outputLength) ++ " bits.")
-- Calulate and print compressionFactor
         let compressionFactor = round $ ((fromIntegral outputLength) / (fromIntegral inputLenghtInBits)) * 100
         putStrLn("factor: " ++ (show outputLength) ++ " / " ++ (show inputLenghtInBits) ++ " * 100% = " ++ (show compressionFactor) ++ "%")
-- Write files and print checks
         putStrLn("Writing " ++ treeFile ++ " to disk...")
         putStrLn("Writing " ++ outputFile ++ " to disk...")
         writeFile treeFile treeTable
         writeFile outputFile outputText;
         putStrLn("Done...")