incr :: Int -> Int
incr x = x+1

triple :: Int -> Int
triple x = 3*x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"

count :: String -> String
count str = show (length str) ++ " characters."

{- a) incr (triple 3)

triple x = 3*x <=> x = 3*3 = 9
incr 9 = 9+1 = 10


-}

{-  b) triple (incr (3+1))

incr (3+1) = incr 4 = 4+1 = 5
triple 5 = 3*5 = 15

-}

{- c) triple (incr 3 + 1)

incr 3 = 3+1 = 4
incr 3 + 1 = 4 + 1 = 5
triple 5 = 3*5 = 15

-}

{- d) triple (incr 3) + 

incr 3 = 3+1 = 4
triple 4 = 3*4 = 12

-}

{- e) welcome "Harry" ++ welcome "Potter"

welcome "Harry" = "Hello, " ++ "Harry" ++ "!" = "Hello, Harry!"
welcome "Potter" = "Hello, " ++ "Potter" ++ "!" = "Hello, Potter!"
welcome "Harry" ++ welcome "Potter" = "Hello, Harry!" ++ "Hello, Potter!"
= "Hello, Harry!Hello, Potter!"

-}

{- f) welcome ("Harry" ++ " Potter")

welcome ("Harry" ++ " Potter") = welcome "Harry Potter"
= "Hello, " ++ "Harry Potter" ++ "!"
= "Hello, Harry Potter!"

-}

{- g) welcome (welcome "Potter")

welcome "Potter" = "Hello, " ++ "Potter" ++ "!" = "Hello, Potter!"
welcome ("Hello, Potter!") = "Hello, " ++ "Hello, Potter!" ++ "!"
= "Hello, Hello, Potter!!"

-}

{- h) count "Expelliarmus!"
length "Expelliarmus!" = 13
count "Expelliarmus!" = show 13 ++ " characters." 
= "13 characters."

-}

{- i) count (count "Expelliarmus!")

count "Expelliarmus!" = "13 characters."
length "13 characters." = 15
= show 15 ++ " characters."
= "15 characters. characters."

-}