import Data.Char (ord, chr)
import Test.QuickCheck

--a
rot13 :: String -> String
rot13 = map rot13Char
    where
        rot13Char c
            | 'a' <= c && c <= 'm' = chr (ord (c) + 13)
            | 'n' <= c && c <= 'z' = chr (ord (c) - 13)
            | 'A' <= c && c <= 'M' = chr (ord (c) + 13)
            | 'N' <= c && c <= 'Z' = chr (ord (c) - 13)
            | otherwise = c

--b
prop_rot13_involution :: String -> Bool
prop_rot13_involution s = (rot13 . rot13) s == s

--compile
main :: IO ()
main = do
    txt <- getLine
    print (rot13 txt)