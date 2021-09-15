-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
                where n = (length xs) `div` 2

-- 2.
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (a : (b : (c : _))) = c

-- 3.
safetail :: [a] -> [a]
safetail xs = if null xs then error "empty list error" else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs      = error "empty list error"
             | otherwise    = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = error "empty list error"
safetail'' xs = tail xs

-- 4.
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or'' :: Bool -> Bool -> Bool
or'' _ True = True
or'' True _ = True

-- 5.
and' :: Bool -> Bool -> Bool
and' x y = if x then if y then True else False else False

-- 6.
and'' :: Bool -> Bool -> Bool
and'' x y = if x then y else False
-- Not really sure what to notice here

-- 7. mult x y z = x * y * z can be seen as (\x -> (\y -> (\z -> x * y * z)))

-- 8.
luhnDouble :: Int -> Int
luhnDouble x 
        | x < 5        = x * 2
        | otherwise    = x * 2 - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a +  b + luhnDouble c + d) `mod` 10 == 0

luhnSum :: [Int] -> Int
luhnSum [] = 0
luhnSum [a] = a
luhnSum xs = luhnSum (init (init xs)) + luhnDouble (xs !! (length xs - 2)) + (xs !! (length xs - 1))

luhn' :: [Int] -> Bool
luhn' xs = luhnSum xs `mod` 10 == 0
