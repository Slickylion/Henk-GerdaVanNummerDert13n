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

push :: Ord a => BinTree a -> a -> BinTree a
push Empty x = Node Empty x Empty
push (Node a y b) x
    | x < y = Node (push a x) y b
    | otherwise = Node a y (push b x)

filterTree :: (a -> Bool) -> BinTree a -> [a]
filterTree f Empty = []
filterTree f (Node leftSubTree a rightSubTree)
    | f a = a : rest
    | otherwise = rest
    where rest = filterTree f leftSubTree ++ filterTree f rightSubTree

pushList :: Ord a => [a] -> BinTree a -> BinTree a
pushList rest b = foldl push b rest

maptree :: (a-> b) -> BinTree a -> BinTree b
maptree _ Empty = Empty
maptree f (Node leftSubTree a rightSubTree) = Node (maptree f leftSubTree) (f a) (maptree f rightSubTree)

preorder :: BinTree a -> [a]
preorder (Node leftSubTree a rightSubTree) =
  [a] ++ preorder leftSubTree ++ preorder rightSubTree
preorder _ = []

postorder :: BinTree a -> [a]
postorder (Node leftSubTree a rightSubTree) =
  postorder leftSubTree ++ postorder rightSubTree ++ [a]
postorder _ = []

inorder :: BinTree a -> [a]
inorder (Node leftSubTree a rightSubTree) =
  inorder leftSubTree ++ [a] ++ inorder rightSubTree
inorder _ = []
-- testing
printTree (Node y a b) = show y
printTree Empty = "This is empty"