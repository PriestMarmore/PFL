import Text.ParserCombinators.ReadPrec (reset)
calcPi1 :: Int -> Double
calcPi1 n = sum(take n(zipWith (/) nums dens1))
    where
        nums = cycle [4,-4]
        dens1 = [1,3..]

calcPi2 :: Int -> Double
calcPi2 n = 3 + sum(take (n-1)(zipWith (/) nums dens2))
    where
        nums = cycle [4,-4]
        dens2 = zipWith (*) st rest
            where
                st = [2,4..]
                rest = zipWith (*) nd rd
                    where
                        nd = [3,5..]
                        rd = [4,6..]

-- Professor's solution
calcPi3 :: Int -> Double
calcPi3 n = 3 + sum(take (n-1)(zipWith (/) nums dens3))
    where
        nums = cycle [4,-4]
        dens3 = [ x*(x+1)*(x+2) | x <- [2,4..]]