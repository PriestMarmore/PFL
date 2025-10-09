pyths :: Integer -> [(Integer, Integer, Integer)]
pyths total = [(x,y,z) | x <- [1..total], y<-[1..total], z<-[1..total], x^2 + y^2 == z^2]
