module Main where
import Data.Char (isAlpha, toLower)

-- Dictionary is a list of strings (words)
type Dict = [String]

-- ANSI escape codes for reverse video mode
reverseOn :: String
reverseOn = "\ESC[7m"

reverseOff :: String
reverseOff = "\ESC[0m"

-- Clean a word by removing punctuation and converting to lowercase
cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha

-- Part (b): Check if a word is in the dictionary
checkWord :: Dict -> String -> String
checkWord dict word
    | null cleaned        = word                          -- empty or just punctuation
    | elem cleaned dict   = word                          -- word found: return as is
    | otherwise          = reverseOn ++ word ++ reverseOff -- word not found: highlight it
    where cleaned = cleanWord word

-- Read dictionary from the system dictionary file
readDict :: IO Dict
readDict = do 
    txt <- readFile "/usr/share/dict/words"
    return (map cleanWord $ words txt)  -- clean dictionary words too!

-- Part (c): Check entire text
spellCheck :: Dict -> String -> String
spellCheck dict text = unlines                           -- 4. Join lines back together
                      [ unwords                          -- 3. Join words in each line
                        [ checkWord dict word            -- 2. Check each word
                        | word <- words line ]           -- 1. Split line into words
                      | line <- lines text ]             -- 0. Split text into lines

-- Main program with tests
main :: IO ()
main = do
    dict <- readDict
    putStrLn $ "Dictionary contains " ++ show (length dict) ++ " words.\n"
    
    -- Test checkWord with some examples
    putStrLn "Testing checkWord function:"
    putStrLn $ "Checking 'hello': " ++ checkWord dict "hello"
    putStrLn $ "Checking 'xyz': " ++ checkWord dict "xyz"
    putStrLn $ "Checking 'test': " ++ checkWord dict "test"
    putStrLn $ "Checking 'haskell': " ++ checkWord dict "haskell"

    -- Test spellCheck with a multi-line text
    putStrLn "\nTesting spellCheck function:"
    let testText = "Hello world! This is a test.\n" ++
                  "Some wrds are misspeled in this txt.\n" ++
                  "But computer and haskell are fine."
    putStrLn "Original text:"
    putStrLn testText
    putStrLn "\nText with spelling checks:"
    putStrLn $ spellCheck dict testText
