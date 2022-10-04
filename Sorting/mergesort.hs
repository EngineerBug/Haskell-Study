halve xs = splitAt (length xs `div` 2) xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

msort [x] = [x]
msort xs =
    let (l,r) = halve xs
    in merge (msort l) (msort r)

main = do
    print (msort [1])           --[1]
    print (msort [5,2,6,23])    --[2,5,6,23]
    print (msort [98,3,36,4])   --[3,4,36,98]
    print (msort [1,2,3,4])     --[1,2,3,4]