-- 1. sum [x^2 | x <- [1..100]]

-- 2.
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- 3.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 4.
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 5.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 6.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], z <- [1..n], y <- [1..n], x^2 + y^2 == z^2]

-- 7.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (factors m) == 2 * m]

-- 8. [(x,y) | x <- [1,2], y <- [4,5]] is equal to concat [[(x,y) | y <- [4,5]] | x <- [1,2]]

-- 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- 10. See CaesarUpperCase.hs