--bubble sort

{-
Arguments: the head of the list and the tail of the list
Output: a list with sorted pairs of elements

Checks if the first two elements are sorted, 
then repeats the function on the tail of the list.
-}
bubpass :: [Int] -> [Int]
bubpass [] = []
bubpass [a] = [a]
bubpass (x:xs)
    | x > head xs = head xs : bubpass (x:tail xs)
    | otherwise = x : bubpass xs

{-
Arguments: an unsorted list
Output: a sorted list

checks if a further pass would equal the input
    -if they are the same, return the input
    -else run again on the current input
-}
bubsort :: [Int] -> [Int]
bubsort [] = []
bubsort [a] = [a]
bubsort xs
    | bubpass xs == xs = xs           --checks if the input will change with another pass
    | otherwise = bubsort (bubpass xs)  --runs another pass if the input will change

main = do
    print (bubsort[0,2,2,5,6,8,15])
    print (bubsort[5,2,6,2,8,0,15])