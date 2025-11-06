data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Show, Enum, Bounded)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show, Enum, Bounded)

data Card = Card Face Suit
    deriving (Eq, Show)

allCards :: [Card]
allCards = [Card face suit | face <- [minBound .. maxBound], suit <- [minBound .. maxBound]]

-- Test functions
countCards :: Int
countCards = length allCards

getHearts :: [Card]
getHearts = filter (\(Card _ s) -> s == Hearts) allCards

getAces :: [Card]
getAces = filter (\(Card f _) -> f == Ace) allCards

main :: IO ()
main = do
    putStrLn "Testing playing cards implementation:"
    putStrLn $ "Total number of cards: " ++ show countCards ++ " (should be 52)"
    putStrLn $ "First 5 cards: " ++ show (take 5 allCards)
    putStrLn $ "All Hearts: " ++ show getHearts
    putStrLn $ "All Aces: " ++ show getAces
    putStrLn $ "Ace of Hearts exists: " ++ show (Card Ace Hearts `elem` allCards)
    putStrLn $ "Two of Clubs exists: " ++ show (Card Two Clubs `elem` allCards)