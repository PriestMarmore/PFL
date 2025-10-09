propDivs :: Integer -> [Integer]
propDivs n = init [x | x <- [1..n], n `mod` x == 0]


isPrime :: Integer -> Bool
isPrime num = 1 == length (propDivs num)