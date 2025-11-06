module Main where

data Set a = Empty
           | Node (Set a) a (Set a)
    deriving (Eq, Show)


example1 :: Set Int
example1 = Empty

example2 :: Set Int
example2 = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)

example3 :: Set Int
example3 = Node (Node (Node Empty 1 Empty) 2 Empty) 4 (Node Empty 6 (Node Empty 7 Empty))

exampleLeftSkew :: Set Int
exampleLeftSkew = Node (Node (Node (Node Empty 1 Empty) 2 Empty) 3 Empty) 4 Empty

exampleRightSkew :: Set Int
exampleRightSkew = Node Empty 1 (Node Empty 2 (Node Empty 3 (Node Empty 4 Empty)))

exampleLongChain :: Set Int
exampleLongChain = Node (Node (Node (Node (Node Empty 1 Empty) 2 Empty) 3 Empty) 4 Empty) 5 Empty

-- a
size :: Set a -> Int
size Empty = 0
size (Node l _ r)   = 1 + size l + size r

-- b
height :: Set a -> Int
height (Node l _ r) = 1 + max (height l) (height r)
height Empty = 0

-- c
set1 = foldr insert empty [1..1000]
set2 = fromList [1..1000]

main :: IO ()
main = do
    -- a
    putStrLn "-- a --"
    putStrLn $ "size 1 (Empty): " ++ show (size example1)
    putStrLn $ "size 2 (3 elements): " ++ show (size example2)
    putStrLn $ "size 3 (5 elements): " ++ show (size example3)
    putStrLn $ "exampleLeftSkew size: " ++ show (size exampleLeftSkew)
    putStrLn $ "exampleRightSkew size: " ++ show (size exampleRightSkew)
    putStrLn $ "exampleLongChain size: " ++ show (size exampleLongChain)
    
    -- b
    putStrLn "\n-- b --"
    putStrLn $ "height 1 (Empty): " ++ show (height example1)
    putStrLn $ "height 2 (example2): " ++ show (height example2)
    putStrLn $ "height 3 (example3): " ++ show (height example3)
    putStrLn $ "exampleLeftSkew height: " ++ show (height exampleLeftSkew)
    putStrLn $ "exampleRightSkew height: " ++ show (height exampleRightSkew)
    putStrLn $ "exampleLongChain height: " ++ show (height exampleLongChain)

    -- c
    putStrLn "\n-- c --"
    putStrLn $ "set1 height (inserts): " ++ show (height set1)
    putStrLn $ "set2 height (fromList): " ++ show (height set2)
