{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
median :: Ord a => a -> a -> a -> a

-- (a)
median x y z
    | (x <= y && y <= z) || (z <= y && y <= x) = y
    | (y <= x && x <= z) || (z <= x && x <= y) = x
    | otherwise = z
-- The most general type for median is the one we're currently using
-- since we don't really care about the specific type of the elements.

-- (b)
median2 :: (Num a, Ord a) => a -> a -> a -> a
median2 x y z = (x + y + z) - (min x (min y z)) - (max x (max y z))
