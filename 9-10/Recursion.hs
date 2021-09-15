-- 1.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 2. We take the convention that 0^0 = 1. Assumed non-negative integral powers
exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' x n = x * (exp' x (n-1))

-- 3.
euclid :: Int -> Int -> Int
euclid a 0 = a
euclid 0 b = b
euclid a b
        | a <= b    = euclid a (b-a)
        | otherwise = euclid (a-b) b

-- 4.
length' :: [a] -> Int
length' [] = 0
length' xs = 1  + length' (tail xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n xs = drop' (n-1) (tail xs)

init' :: [a] -> [a]
init' [_] = []
init' xs = [head xs] ++ (init' (tail xs))

-- 5.
and' :: [Bool] -> Bool
and' [] = True
and' xs = if (head xs) then (and' (tail xs)) else False

concat' :: [[a]] -> [a]
concat' [xs] = xs
concat' xss = (head xss) ++ concat'(tail xss)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

(!!!) :: [a] -> Int -> a
xs !!! 0 = head xs
xs !!! n = (tail xs) !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [y] = x == y
elem' x ys = if x == (head ys) then True else (elem' x (tail ys))

-- 6.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge (x:xs) (y:ys)     | y <= x        =  y : merge (x:xs) ys
                        | otherwise     =  x : merge xs (y:ys)

-- 7.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) 
                where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort f) (msort s)
            where (f, s) = halve xs

-- 8.
sum' :: Num a => [a] -> a
sum' [] = 0
sum' xs = (head xs) + sum'(tail xs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n xs = [(head xs)] ++ take' (n-1) (tail xs)

last' :: [a] -> a
last' [x] = x
last' xs = last' (tail xs)
