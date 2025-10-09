-- a
mymerge :: Ord a => [a] -> [a] -> [a]
mymerge [] ys = ys
mymerge xs [] = xs
mymerge (x:xs) (y:ys)
    | x <= y    = x : mymerge xs (y:ys)
    | otherwise = y : mymerge (x:xs) ys

-- b
mymsort :: Ord a => [a] -> [a]
mymsort [] = []
mymsort [x] = [x]
mymsort xs = mysplit xs
    where
        mysplit xs = mymerge (mymsort left) (mymsort right)
            where
                (left, right) = splitAt (length xs `div` 2) xs