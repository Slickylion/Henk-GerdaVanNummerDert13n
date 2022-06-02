import System.IO  
import System.Environment
import System.Directory
import Data.Function (on)
import Data.List (sortBy)
import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as DM

type HC = [(Int, Char)]
--Reversed for Data.Map.! function

decode :: HC -> [Int] -> String
decode t i = let m = DM.fromList t
                          in map (m DM.!) i

bytestringToList :: HC -> String -> String -> [Int]
bytestringToList t [] str = []
bytestringToList t (x:xs) str = let n = stringToInt (str++[x])
                                                  in if DM.member n (DM.fromList t)
                                                          then [n]++(bytestringToList t xs "")
                                                          else bytestringToList t xs (show n) 

treeToCodeTable :: [String] -> HC
treeToCodeTable [] = []
treeToCodeTable (x:xs) = let c = head x
                         in [(stringToInt $ tail x, c)]++(treeToCodeTable xs)

stringToInt :: String -> Int
stringToInt = read

main = do
          args <- getArgs
          let inputFile = head args
              outputFile = args!!1
              treeFile = args!!2
          inputText <- readFile inputFile
          treeText <- readFile treeFile
          let codeTable = treeToCodeTable (splitOn "," treeText)
              inputList = bytestringToList codeTable inputText ""
              outputText = decode codeTable inputList
              outputLength = length outputText
              outputLengthInBits = outputLength * 8
          putStrLn("Length of decompressed file: " ++ (show outputLength) ++ " characters, " ++ (show outputLengthInBits) ++ " bits.")
          putStrLn("Writing " ++ outputFile ++ " to disk...")
          writeFile outputFile outputText
          putStrLn("Done...")