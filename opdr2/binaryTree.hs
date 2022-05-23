-- Gemaakt door:
-- Ben Derksen (1014934)
-- Marjon Buijing (1015470)

-- pre order traversal:
-- in order traversal:
-- post order traversal:
module Main where


main :: IO ()
main = do
    let x = Node 0 Empty Empty;
    putStr "binary Tree"
    

data BinTree a = Empty
    | Node a (BinTree a)(BinTree a)
     deriving (Eq, Ord, Show, Read)


push :: (Ord a) => BinTree a -> a -> BinTree a
push Empty x = Node x Empty Empty
push (Node y a b) x = Node y (Node x Empty Empty) b


-- testing
printTree (Node y a b) = show y
printTree Empty = "This is empty"



