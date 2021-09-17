-- 1.
{-
1 + (2*3) outermost
2*3 innermost

(1+2) * (2+3) outermost
1+2 innermost
2+3 innermost

fst(1+2, 2+3) outermost
1+2 innermost
2+3 innermost

(\x -> 1 + x)(2*3) outermost
1 + (2*3) both
2*3 innermost
-}

-- 2. fst (1+2, 2+3) = 1+2 = 3. It only calculates 1+2 and ignors 2+3

-- 3.
{-
mult 2 3
=   (\x -> (\y -> x*y)) 2 3
=   (\y -> 2*y) 3
=   2 * 3
=   6
-}

-- 4.
fibs :: [Integer]
fibs = [0,1] ++ [x + y | (x,y) <- zip fibs (tail fibs)]

-- 5.
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeat' :: a -> Tree a
repeat' x = xs 
            where xs = Node xs x xs

take' :: Int -> Tree a -> Tree a
take' 0 _    = Leaf
take' _ Leaf = Leaf
take' n (Node l x r) = Node (take' (n-1) l) x (take' (n-1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' (n-1) . repeat'

-- 6.

sqroot :: Double -> Double
sqroot x = withinDistance xs
            where   withinDistance (x:(y:xs))   | abs(x - y) < distance = y
                                                | otherwise             = withinDistance (y:xs)
                        where distance = 0.01
                    xs = iterate (next x) x
                        where next n a = (a + n/a) / 2