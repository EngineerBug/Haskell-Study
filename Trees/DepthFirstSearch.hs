--Monomorphic binary tree
data Tree = Leaf Int | Node Tree Int Tree deriving (Show, Read)

tree :: Tree
tree = Node (Node (Leaf 6) 5 (Leaf 8)) 10 (Node (Leaf 2) 15 (Leaf 45))

numberedTree :: Tree
numberedTree = Node ( Node (Leaf 4) 2 (Leaf 5) ) 1 ( Node (Leaf 6) 3 (Leaf 7) )

{-
Arguments:
    - a tree to search
    - a value to search for

If the tree is a leaf then check if its value is the goal value
If the tree is a node, check the value then search each branch

Output:
    - true, if the value is present in the tree
    - else false.
-}
dfs :: Tree -> Int -> Bool
dfs (Leaf n) x
    | n == x = True
    | otherwise = False
dfs (Node t1 n t2) x
    | n == x = True
    | otherwise = dfs t1 x || dfs t2 x

{-
Arguments:
    - a tree to search
    - a value to search for

If the tree is a leaf:
    If the value equals the goal, return a singlton list with the value
    Else return an empty list
If the tree is a node:
    If the value equals the goal, return a singlton list with the value
    If the left tree contains the value (using dfs), search the left tree
    If the right tree contains the value (using dfs), search the right tree
    Else return an empty list

Output: a list of node values that lead from the top to the goal node
-}
dfsPath :: Tree -> Int -> [Int]
dfsPath (Leaf n) x
    | n == x = [n]
    | otherwise = []
dfsPath (Node t1 n t2) x
    | n == x = [n]
    | dfs t1 x && x /= n = n : dfsPath t1 x 
    | dfs t2 x && x /= n = n : dfsPath t2 x
    | otherwise = []

main = do
    print(dfs tree 15) -- True
    print(dfs numberedTree 12) -- False
    print(dfsPath tree 45) -- [10, 15, 45]
    print(dfsPath numberedTree 2) -- [1, 2]
    print(dfsPath numberedTree 18) -- []