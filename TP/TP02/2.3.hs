import Test.QuickCheck

max, min :: Ord a => a -> a -> a
max x y = if x >= y then x else y
min x y = if x <= y then x else y

-- (a)
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z
  | x>=y = if x>=z then x else z
  | y>=z = y
  | otherwise = z
min3 x y z
  | x<=y = if x<=z then x else z
  | y<=z = y
  | otherwise = z

-- (b)
max3_2, min3_2 :: Ord a => a -> a -> a -> a
max3_2 x y z = Prelude.max x (Prelude.max y z)
min3_2 x y z = Prelude.min x (Prelude.min y z)
