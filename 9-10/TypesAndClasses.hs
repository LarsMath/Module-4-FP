-- 1.
{-
    [Char]
    (Char, Char, Char)
    [(Bool, Char)]
    ([Bool], [Char])
    [([a] -> [a])]
-}

-- 2.
bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[0]]

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

-- 3.
{- 
    second :: [a] -> a
    swap :: (a,b) -> (b,a)
    pair :: a -> b -> (a,b)
    double :: Num a => a -> a
    palindrome :: Eq a => [a] -> Bool
    twice :: (a -> a) -> a -> a       (ghci says t, what is convention?)
-}

-- 4. It is not feasible if the domain (or at least the domain on which the function is defined) is 
--    infinite. An example of a family of arrow types that could implement Eq is (Bool -> a)