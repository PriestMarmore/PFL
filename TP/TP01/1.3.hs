second :: [a] -> a
second xs = head (tail xs)

last1 :: [a] -> a
last1 xs = head (reverse xs)
last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)
-- last = head (reverse) = xs !! (length xs - 1)

init1 :: [a] -> [a]
init1 xs = reverse (tail (reverse xs))
init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs
-- init = reverse(tail(reverse)) = take (length xs - 1) xs

middle :: [a] -> a
middle xs = head(drop (length xs `div` 2) xs)
--with an even number it give the (n/2) position

checkPalindrome :: (Eq a) => [a] -> Bool
checkPalindrome xs = xs == reverse xs