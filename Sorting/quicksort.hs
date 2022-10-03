--quicksort
quicksort :: [Int] -> [Int]
--catch sorts when they reach an empty or single element list
quicksort [] = []
quicksort [a] = [a]
quicksort xs = 
    --choose the pivot
    let z = xs !! (length xs `div` 2) 
    --sort [all elements smaller than the pivot] ++ [all elements equal to the pivot] ++ sort [all elements larger than the pivot]
    in quicksort [x | x <- xs, x < z] ++ [x | x <- xs, x == z] ++ quicksort [x | x <- xs, x > z]

{-
by the way, "let" and "in" are technically a single line
"let" will define variables within the scope of the "in" statement
-}

main = do
    print (quicksort [5,8,4,9,1,8,3])