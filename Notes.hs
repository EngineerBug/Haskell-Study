par :: a -> b -> b
par x y = y

--parallel maximum function
dcmax :: [ Int ] -> Int
dcmax [x] = x
dcmax xs = 
  par dys (par dzs (
    if dys > dzs
    else dzs
  ))
  where
    mid = length xs `div` 2
    (ys, zs) = splitAt mid xs
    dys = dcmax ys
    dzs = dcmax zs