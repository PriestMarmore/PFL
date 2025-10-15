divisors :: Integer -> [Integer]
--divisors n = [d | d<-[1..n], n‘mod‘d == 0]
divisors n = filter (\d -> n `mod` d == 0) [1..n]