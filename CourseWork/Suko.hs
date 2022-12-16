import Data.List ( permutations )

index :: (Eq a) => Int -> [a] -> a
index x [e] = e
index 0 (x:xs) = x
index i (x:xs) = index (i-1) xs

indices :: (Eq a) => [Int] -> [a] -> [a]
indices [] xs = []
indices is [] = []
indices (i:is) xs = index i xs : indices is xs

possibles :: [[Int]]
possibles = permutations [1..9]

acceptable :: Int -> Int -> Int -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Bool
acceptable t1 t2 t3 t4 us u vs v ws w grid = 
       t1 == sum (indices [0,1,3,4] grid)
    && t2 == sum (indices [1,2,4,5] grid)
    && t3 == sum (indices [3,4,6,7] grid)
    && t4 == sum (indices [5,6,8,9] grid)
    && u == sum us
    && v == sum vs
    && w == sum ws

--find the item in possible that matches the elements in acceptable
suko :: Int -> Int -> Int -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [[Int]]
suko t1 t2 t3 t4 us u vs v ws w = filter (acceptable t1 t2 t3 t4 us u vs v ws w) possibles

main = do
    print (suko 19 10 23 19 [7,2,3,6] 18 [4,1,9] 14 [8,5] 13)