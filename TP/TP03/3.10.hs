isPrimeFast :: Integer -> Bool
isPrimeFast n = all (\x -> n `mod` x /= 0) [2..(floor (sqrt (fromIntegral n)))]