{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
safetail :: [a] -> [a]

-- Conditional expression
safetail xs = if null xs then [] else tail xs

-- Guards
safetail xs
  | null xs   = []
  | otherwise = tail xs

-- Patterns
safetail [] = []
safetail (_:xs) = xs