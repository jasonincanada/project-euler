-- projecteuler.net problem 1
-- Jason Hooper

f :: Int -> Bool
f x = mod x 3 == 0 || mod x 5 == 0

p1 :: Int
p1 = sum $ filter f [1..999]
