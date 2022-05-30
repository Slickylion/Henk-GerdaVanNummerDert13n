-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)

-- pre order traversal:
-- in order traversal:
-- post order traversal:
module Main where

main :: IO ()
main = do
    let x = Node Empty 0 Empty
    putStr "binary Tree"

data BinTree a = Empty
    | Node (BinTree a) a (BinTree a)
     deriving (Eq, Ord, Show, Read)

-- als waarde hoger dan boomwaarde, ga naar rechts anders links

push Empty x = Node Empty x Empty
push (Node a y b) x
    | x < y = Node (push a x) y b
    | otherwise = Node a y (push b x)
    
mapTree :: (t -> a) -> BinTree t -> BinTree a
mapTree _ Empty =  Empty
mapTree f (Node leftTree a rightTree) = Node (mapTree f leftTree) (f a) (mapTree f rightTree)

-- testing
printTree (Node y a b) = show y
printTree Empty = "This is empty"



