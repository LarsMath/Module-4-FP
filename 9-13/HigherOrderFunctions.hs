-- 1.
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : (map'' f xs)

-- 2.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x:xs)   | p x       = x : (filter'' p xs)
                    | otherwise = filter'' p xs

-- 3. [f x | x <- xs, p x] /cong map f . filter p

-- 4.
all' :: (a -> Bool) -> [a] -> Bool
all' p = null . filter (not . p)

any' :: (a -> Bool) -> [a] -> Bool
any' p = not . null . filter p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x       = x : (takeWhile' p xs)
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = (x:xs)

-- 5.
map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\x ys -> f x : ys) []

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p = foldr (\x ys -> if p x then x:ys else ys) []

-- 6.
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

-- 7. sum is not [a] -> [a] and does not fit into the [[a] -> [a]] array

-- 8.
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = (\x y -> f (x,y))

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = (\(x,y) -> f x y)

-- 9.
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold (null) (take 8) (drop 8)

map'''' :: (a -> b) -> [a] -> [b]
map'''' f = unfold (null) (f . head) (tail)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) (id) (f)

-- 10. See ParityBinaryTransmitter.hs

-- 11. See ParityBinaryTransmitter.hs

-- 12.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : (altMap g f xs)

--13.
luhnDouble :: Int -> Int
luhnDouble x 
        | x < 5        = x * 2
        | otherwise    = x * 2 - 9

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble (reverse xs)) `mod` 10 == 0