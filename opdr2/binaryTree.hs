-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)

-- pre order traversal:
-- in order traversal:
-- post order traversal:
module Main where


main = do
    let x = Node 0 Empty Empty;
    putStrLn $ show x

data BinTree a = Empty
    | Node a (BinTree a)(BinTree a)
     deriving (Show, Read)


push :: a -> BinTree a -> BinTree a
push x Empty = Node x Empty Empty
push x (Node y a b) = Node y (Node x Empty Empty) b









