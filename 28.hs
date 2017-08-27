-- projecteuler.net problem 28
-- Jason Hooper

-- Calculate the contribution of ring n to the overall sum 
ring :: Int -> Int
ring 1 = 1
ring n = let ns = n*n
         in ns + (ns - 1*(n-1))
               + (ns - 2*(n-1))
               + (ns - 3*(n-1))
		
-- Given an odd number representing the outermost ring, spiral 
-- inwards until we get to the center ring (n = 1)
calc :: Int -> Int
calc 1 = 1
calc n = ring n + calc (n-2)

p28 :: Int
p28 = calc 1001

