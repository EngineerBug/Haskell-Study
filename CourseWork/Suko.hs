import Data.List ( permutations )

index :: (Eq a) => Int -> [a] -> a
index x [e] = e
index 0 (x:xs) = x
index i (x:xs) = index (i-1) xs

indices :: (Eq a) => [Int] -> [a] -> [a]
indices [] xs = []
indices is [] = []
indices (i:is) xs = index i xs : indices is xs

--all permutations of 1..9
possibles :: [[Int]]
possibles = permutations [1..9]

{-
Arguments: 
    the four numebrs between squares
    the three sums of the coloured boxed
    a possible permutation of 1..9

Check that the corner digits sum to T0,1,2,3
Check that colourd digits sum to U, V and W

Output: true/false
-}
acceptable :: Int -> Int -> Int -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Bool
acceptable t1 t2 t3 t4 us u vs v ws w grid = 
       t1 == sum (indices [0,1,3,4] grid)
    && t2 == sum (indices [1,2,4,5] grid)
    && t3 == sum (indices [3,4,6,7] grid)
    && t4 == sum (indices [4,5,7,8] grid)
    && u == sum (indices us grid)
    && v == sum (indices vs grid)
    && w == sum (indices ws grid)

--find the item in possible that matches the criteria in acceptable
suko :: Int -> Int -> Int -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [[Int]]
suko t1 t2 t3 t4 us u vs v ws w = filter (acceptable t1 t2 t3 t4 us u vs v ws w) possibles

main = do
    print (suko 19 10 23 19 [0,1,2,3] 18 [4,5,8] 14 [6,7] 13)