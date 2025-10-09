propDivs :: Integer -> [Integer]
propDivs n = init [x | x <- [1..n], n `mod` x == 0]