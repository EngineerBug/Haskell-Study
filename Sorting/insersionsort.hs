{-
insertElement
Arguments:
    x:xs a list ending with the element that needs to be inserted

Are the first two elements in the correct order?
    Yes: return the list unaltered
    No: swap the first two elements and run on the tail of the list

Output:
    a list with the last element moved forward until all preceding 
    elements are smaller than the inseted element.
-}
insertElement :: [Int] -> [Int]
insertElement [] = []
insertElement [a] = [a]
insertElement (x:z:xs)
    | x > z = z : insertElement (x:xs)
    | otherwise = x:z:xs
{-
insertionSort
Arguments:
    xs a list of unordered integers

Output:
    a sorted list of integers
-}
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insertElement (x : insertionSort xs)

main = do
    print (insertionSort [3,5,2,1,4])