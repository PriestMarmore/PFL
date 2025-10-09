{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Test.QuickCheck
short :: [a] -> Bool

-- (a)
short xs = length xs <= 3

-- (b)
short []       = True
short [_]      = True
short [_,_]    = True
short [_,_,_]  = True
short _        = False
