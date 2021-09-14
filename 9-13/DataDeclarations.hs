-- 1.
data Nat = Zero | Succ Nat
            deriving (Eq, Ord, Show, Read)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = add n (Succ m)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ n) m = add m (mult n m)

-- 2.
data Tree' = Leaf' Int | Node' Tree' Int Tree'
                deriving (Eq, Ord, Show, Read)

occurs :: Int -> Tree' -> Bool
occurs m (Leaf' n) = m == n
occurs m (Node' l n r) = case (compare m n) of
                                EQ -> True
                                LT -> occurs m l
                                GT -> occurs m r

-- 3. Wait, this does not work? This tree is always 
data Tree = Leaf Int | Node Tree Tree
                deriving (Eq, Ord, Show, Read)
balance :: [Int] -> Tree
balance [n] = Leaf n
balance xs = Node (balance (take n xs)) (balance (drop n xs))
                where n = (length xs) `div` 2

-- 4.
maxDiffOne :: Int -> Int -> Bool
maxDiffOne n m = n == m + 1 || n == m - 1 || (n == m)

balancedSubtree :: Tree -> (Bool, Int)
balancedSubtree (Leaf _) = (True, 1)
balancedSubtree (Node l r) = (lb && rb && (maxDiffOne ll rl), ll + rl)
                            where   (lb, ll) = balancedSubtree l
                                    (rb, rl) = balancedSubtree r

balanced :: Tree -> Bool
balanced = fst . balancedSubtree

-- 5. Typo in data declaration in exercises
data Expr = Val Int | Add Expr Expr
            deriving (Eq, Ord, Show, Read)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6.
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- 7.
instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x == Just y = x == y

same :: Eq a => [a] -> Bool
same [] [] = true
same (x:xs) (y:ys) = (x == y) && (same xs ys)

instance Eq a => Eq [a] where
    (==) xs ys = same xs ys