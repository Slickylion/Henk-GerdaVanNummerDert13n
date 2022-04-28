-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)
module Huilon where
import System.Environment

import Data.Char
main :: IO ()
main = do 
    text <- readFile "HalloGerda.txt"
    let processed = text
    writeFile "DagGerrit.txt" processed
    putStrLn processed
