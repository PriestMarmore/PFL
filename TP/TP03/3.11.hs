-- a
myappend :: [a] -> [a] -> [a]
myappend xs ys = foldr (:) ys xs

-- b
myconcat :: [[a]] -> [a]
myconcat xss = foldr (++) [] xss

-- c
myreverse :: [a] -> [a]