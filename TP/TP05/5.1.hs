data List a = Empty | Cons a (List a)

toList :: [a] -> List a
toList []     = Empty
toList (x:xs) = Cons x (toList xs)


fromList :: List a -> [a]
fromList Empty        = []
fromList (Cons x xs)  = x : fromList xs

main :: IO ()
main = do
    print (fromList (toList ([] :: [Int])))     -- prints: []   
    print (fromList (toList [1,2,3]))           -- prints: [1,2,3]
    print (fromList (Cons 4 (Cons 5 Empty)))    -- prints:
    print (fromList (toList ["a","b"]))         -- prints: ["a","b"]