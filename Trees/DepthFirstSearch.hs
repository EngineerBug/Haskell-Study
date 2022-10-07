--Monomorphic tree
data Tree = Leaf | Node Tree Int Tree deriving (Show, Read)

x :: Tree
x = Node Leaf 20 Leaf

y :: Tree
y = Node (Node Leaf 5 Leaf) 10 (Node Leaf 15 Leaf)