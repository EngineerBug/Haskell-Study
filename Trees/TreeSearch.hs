--Polymorphic tree
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Read)

search :: (Eq a, Ord a)  => a -> Tree a -> Bool
search z (Leaf b) = False
search x (Node b y c)
    | x < y = search x b
    | x > y = search x c
    | otherwise = True

x :: Tree Int
x = Node (Leaf 12) 20 (Leaf 3)

y :: Tree Int
y = Node (Node (Leaf 3) 5 (Leaf 6)) 10 (Node (Leaf 5) 15 (Leaf 8))

main = do
    print (show x)      --"Node Leaf 20 Leaf"
    print (search 5 y)  --True
    print (search 13 x) --False