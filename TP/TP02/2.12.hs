myconcat :: [[a]] -> [a]
myconcat biglist = [element | sublist <- biglist, element <- sublist]

myreplicate :: Int -> a -> [a]
myreplicate times num = [num | _ <- [1..times]]

myindex :: [a] -> Int -> a
myindex element n = head [x | (x,i) <- zip element [0..], i == n]