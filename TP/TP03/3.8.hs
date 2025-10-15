fromBits :: [Int] -> Int
fromBits bits = myFunc (bits)
  where
    myFunc [] = 0
    myFunc (x:xs)
      | x == 1    = 2 ^ (length xs) + myFunc xs
      | otherwise = myFunc xs
