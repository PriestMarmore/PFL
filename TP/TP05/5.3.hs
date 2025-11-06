module Main where
import Data.List (sortBy)

-- 5.3.hs
data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Show, Enum, Bounded)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show, Enum, Bounded)

data Card = Card Face Suit
    deriving (Eq, Show)

-- allCards: full deck (13 faces x 4 suits = 52 cards)
allCards :: [Card]
allCards = [Card f s | f <- [minBound .. maxBound], s <- [minBound .. maxBound]]

-- a
data MyOrdering = MyLT | MyEQ | MyGT
    deriving (Eq, Show)

cmp1 :: Card -> Card -> MyOrdering
cmp1 (Card _ suit1) (Card _ suit2) = case (suit1, suit2) of
    (Clubs, Clubs)       -> MyEQ
    (Clubs, _)          -> MyLT
    (_, Clubs)          -> MyGT
    (Spades, Spades)    -> MyEQ
    (Spades, _)         -> MyLT
    (_, Spades)         -> MyGT
    (Hearts, Hearts)    -> MyEQ
    (Hearts, _)         -> MyLT
    (_, Hearts)         -> MyGT
    (Diamonds, Diamonds) -> MyEQ

-- b
cmp2 :: Card -> Card -> MyOrdering
cmp2 (Card f1 s1) (Card f2 s2)
    | fromEnum f1 < fromEnum f2 = MyLT
    | fromEnum f1 > fromEnum f2 = MyGT
    | otherwise                 = cmp1 (Card f1 s1) (Card f2 s2)

-- Convert our MyOrdering to Prelude's Ordering so we can use sortBy
myToPrelude :: MyOrdering -> Prelude.Ordering
myToPrelude MyLT = LT
myToPrelude MyEQ = EQ
myToPrelude MyGT = GT

-- Comparators returning Prelude.Ordering usable with Data.List.sortBy
cmp1Ord :: Card -> Card -> Prelude.Ordering
cmp1Ord a b = myToPrelude (cmp1 a b)

cmp2Ord :: Card -> Card -> Prelude.Ordering
cmp2Ord a b = myToPrelude (cmp2 a b)


main :: IO ()
main = do

    print "------------a-------------"
    putStrLn "Testing card comparison (cmp1):"
    putStrLn "Comparing Ace of Diamonds with Two of Clubs:"
    print $ cmp1 (Card Ace Diamonds) (Card Two Clubs)  -- Should be MyGT (Diamonds comes after Clubs)
    putStrLn "Comparing Two of Clubs with King of Hearts:"
    print $ cmp1 (Card Two Clubs) (Card King Hearts)   -- Should be MyLT (Clubs comes before Hearts)
    putStrLn "Comparing Queen of Spades with King of Hearts:"
    print $ cmp1 (Card Queen Spades) (Card King Hearts) -- Should be MyLT (Spades comes before Hearts)

    print "------------b-------------"
    putStrLn "Testing card comparison (cmp2):"
    putStrLn "Comparing Two of Clubs with Two of Hearts (same face, suit order):"
    print $ cmp2 (Card Two Clubs) (Card Two Hearts)    -- Should be MyLT (Clubs before Hearts)
    putStrLn "Comparing Two of Clubs with Three of Clubs (face order):"
    print $ cmp2 (Card Two Clubs) (Card Three Clubs)   -- Should be MyLT (2 before 3)
    putStrLn "Comparing Ace of Spades with Ace of Diamonds (same face, suit order):"
    print $ cmp2 (Card Ace Spades) (Card Ace Diamonds) -- Should be MyLT (Spades before Diamonds)
    putStrLn "Comparing Ace of Hearts with Ace of Hearts (equal):"
    print $ cmp2 (Card Ace Hearts) (Card Ace Hearts)   -- Should be MyEQ
    
    -- Demonstrate sorting the full deck using the two comparators
    putStrLn "\nSorting full deck by cmp1 (suits grouped):"
    print $ take 8 (sortBy cmp1Ord allCards)
    putStrLn "..."
    putStrLn "Sorting full deck by cmp2 (faces grouped, suits within face):"
    print $ take 8 (sortBy cmp2Ord allCards)
    putStrLn "..."
