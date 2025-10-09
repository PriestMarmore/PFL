--a 
leastDiv :: Integer -> Integer
leastDiv n = findDiv n 2
    where
        findDiv n d
            | d*d >= n = n
            | n `mod` d == 0 = d
            | otherwise = findDiv n (d+1)

--b
isPrimeFast :: Integer -> Bool
isPrimeFast n
    | n < 2 = False
    | otherwise = leastDiv n == n