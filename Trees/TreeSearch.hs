--Monomorphic tree
data Tree = Leaf | Node Tree Int Tree deriving (Show, Read)

search :: Int -> Tree -> Bool
search z Leaf = False
search x (Node b y c)
    | x < y = search x b
    | x > y = search x c
    | otherwise = True

x :: Tree
x = Node Leaf 20 Leaf

y :: Tree
y = Node (Node Leaf 5 Leaf) 10 (Node Leaf 15 Leaf)

main = do
    print (show x)      --"Node Leaf 20 Leaf"
    print (search 5 y)  --True
    print (search 13 x) --False