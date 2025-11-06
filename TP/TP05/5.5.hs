module Main where

import Data.Char (isAlpha, toLower)
import TreeSet (Set, fromList, member)

-- Dictionary represented as a Set of cleaned words
type Dict = Set String

-- ANSI escape codes for reverse video highlighting
reverseOn :: String
reverseOn = "\ESC[7m"

reverseOff :: String
reverseOff = "\ESC[0m"

-- Clean a token: remove non-letters and lowercase
cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha

-- Check a single word (preserve punctuation/case in output)
checkWord :: Dict -> String -> String
checkWord dict word
    | null cleaned        = word
    | member cleaned dict = word
    | otherwise           = reverseOn ++ word ++ reverseOff
  where
    cleaned = cleanWord word

-- Spell-check a whole text: split into lines and words, check each word
spellCheck :: Dict -> String -> String
spellCheck dict txt = unlines [ unwords [ checkWord dict w | w <- words ln ] | ln <- lines txt ]

-- Read dictionary file and build a Set of cleaned words
readDict :: FilePath -> IO Dict
readDict path = do
    txt <- readFile path
    return (fromList (map cleanWord (words txt)))

-- Main: read dictionary, read stdin, output checked text
main :: IO ()
main = do
    dict <- readDict "/usr/share/dict/words"
    input <- getContents
    putStr (spellCheck dict input)
