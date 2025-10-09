--a
myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : myinsert x ys

--b
myisort :: Ord a => [a] -> [a]
myisort [] = []
myisort (x:xs) = myinsert x (myisort xs)