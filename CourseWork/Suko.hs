indices :: (Eq a) => [Int] -> [a] -> [a]
indices [] xs = []
indices is [] = []
indices (i:is) (x:xs)
    | i == 0 = x : indices (is) xs
    | otherwise = indices (i:is) xs

main = do
    print (map (+5) [1,2,3])