-- 1. double (double 2) = double 2 + double 2 = 2 + 2 + double 2 = 4 + double 2 = 4 + (2 + 2) = 4 + 4 = 8

-- 2. sum [x] = x + sum [] = x + 0 = x

-- 3.
product' :: Num a => [a] -> a
product' [] = 1
product' xs = head xs * product' (tail xs)

-- 4. switch the places of smallest and biggest

-- 5. when x is picked as pivot point then duplicate entries will not be considered and the sorted list will have only one entry of x

-- 6.
last' :: [a] -> a
last' = head . reverse

--7.
init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x : (init' xs)
