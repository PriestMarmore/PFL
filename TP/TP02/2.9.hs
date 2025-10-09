import Test.QuickCheck

-- From the previous exercise
propDivs :: Integer -> [Integer]
propDivs n = init [x | x <- [1..n], n `mod` x == 0]


perfects :: Integer -> [Integer]
perfects original = [n | n <- [1..original], sum (propDivs n) == n]
