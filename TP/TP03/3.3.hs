nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub[y | y <- xs, y /= x]