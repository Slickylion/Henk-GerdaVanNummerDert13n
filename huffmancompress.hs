import System.Environment
import Data.Function (on)
import Data.List (sortBy)
import Data.Map

type HC = [(Int, Char)]

makeCodeTable :: HC -> String -> HC
makeCodeTable (x:xs) str = if str == []
                             then let c = snd x
                                      code = "0"
                                 in [(stringToInt code, c)]++(makeCodeTable xs code)
                             else if xs /= []
                                     then let c = snd x
                                              code = '1':str
                                         in [(stringToInt code, c)]++(makeCodeTable xs code)
                                     else let c = snd x
                                              code = addToback (init str) '1'
                                         in [(stringToInt code, c)]

addToback :: String -> Char -> String
addToback str c = str ++ [c]

stringToInt :: String -> Int
stringToInt = read

sortTable :: HC -> HC
sortTable x = sortBy (compare `on` fst) x 

makeTable :: String -> HC
makeTable [] = []
makeTable str = let c = head str
                    n = countChar str c
                    afterStr = removeChar str [c]
                in [(n,c)]++(makeTable afterStr)

countChar :: String -> Char -> Int
countChar str c = length $ filter (== c) str

removeChar :: String -> String -> String
removeChar str c = [x | x <- str, not (x `elem` c)]
-- elem doesn't work with char, so string it is

tupleToString :: (Int, Char) -> String
tupleToString (n, c) = "-" ++ show n ++ "-" ++ (c:"-")


huffmanCompress :: HC -> String -> String
huffmanCompress x str = 

main = do
-- Get the command line arguments and split them into two
         args <- getArgs
         let inputFile = head args
             outputFile = args!!1
             treeFile = args!!2
-- Open input file and compress inputText into outputText
         inputText <- readFile inputFile;
         let treeText = sortTable $ makeTable inputText
             treeTextString = unlines . map tupleToString $ treeText
         writeFile treeFile treeTextString