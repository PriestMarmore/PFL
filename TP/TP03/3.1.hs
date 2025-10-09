-- a

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

--b
myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

--c
myconcat :: [[a]] -> [a]
myconcat (list:lists) = list ++ myconcat lists
myconcat [] = []

--d
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x 
    | n > 0 = x : myreplicate (n-1) x

--e
myExc :: [a] -> Int -> a
myExc (x:_) 0 = x
myExc (_:xs) n 
    | n > 0 = myExc xs (n-1)

--f
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem y (x:xs)
    | y == x = True
    | otherwise = myelem y xs

