twinPrimes :: [(Integer, Integer)]
twinPrimes = [(p,p+2) | p <- primes, isPrime (p+2)]
    where
        primes = sieve [2..]
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
        isPrime n = null [x | x <- [2..limit], n `mod` x == 0]
            where
                limit = floor (sqrt (fromIntegral n))
        