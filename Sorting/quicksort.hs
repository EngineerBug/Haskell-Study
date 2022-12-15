--quicksort

badsort :: [Int] -> [Int]
--catch sorts when they reach an empty or single element list
badsort [] = []
badsort xs = 
    --choose the pivot
    let z = xs !! (length xs `div` 2) 
    --sort [all elements smaller than the pivot] ++ [all elements equal to the pivot] ++ sort [all elements larger than the pivot]
    in badsort [x | x <- xs, x < z] ++ [x | x <- xs, x == z] ++ badsort [x | x <- xs, x > z]

{-
by the way, "let" and "in" are technically a single line
"let" will define variables within the scope of the "in" statement
-}
quickpass :: [Int] -> ([Int],[Int])
quickpass [] = ([],[])
quickpass (x:xs)
    | x > p = (as,x:bs)
    | otherwise = (x:as,bs)
    where 
        p = head (drop (length (x:xs) `div` 2) (x:xs))
        as = fst (quickpass xs)
        bs = snd (quickpass xs)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort xs = quicksort (fst (quickpass xs)) ++ quicksort (snd (quickpass xs))

main = do
    print (quicksort [5,8,4,9,1,8,3])
    print (quicksort [2,8,1,5,3,2,12])