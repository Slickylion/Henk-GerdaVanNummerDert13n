-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)
module Main where
import Data.List
import Data.Char
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then putStrLn "please give file to read and file to write to"
        else do
            let command1 = head args
            let command2 = args !! 1
            text <- readFile command1
            let processed = sort text
            writeFile command2 processed
            putStrLn processed

