-- arithmetic ops

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Int -> Int -> Bool
-- Here I ignore the fact that overflow is not part of real world arithmetic in favor of computing time.
valid Add x y = x <= y
valid Sub _ _ = True
valid Mul x y = x /= 0 && y /= 0 && x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0 && (y^2 /= x)
valid Exp x y = y >= 0 && y /= 1 && x /= 1

-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0
-- valid Exp x y = y >= 0

-- All integers
-- valid Add _ _ = True
-- valid Sub _ _ = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0
-- valid Exp x y = y >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

-- Numeric expressions

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = paren l ++ show o ++ paren r
                     where
                         paren (Val n) = show n
                         paren e       = "(" ++ show e ++ ")"

-- show (App Add (Val 1) (App Mul (Val 2) (Val 3)))

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- use lists to handle failures (invalid expressions)
-- this is also where performance stupid really starts
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- combinatorial functions
-- subsequences
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- insert one element in each possible position in a list
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- permutations
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- choices 1.
choices :: [a] -> [[a]]
choices xs = [x |   y <- subs xs,
                    x <- perms y]

-- formalising the problem
-- we now have the tools to check if an expression is a solution to the problem.
-- this is not necessary to actually brute-force the solution
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

testExpr :: Expr
testExpr = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

-- test: solution testExpr [1,3,7,10,25,50] 765

-- brute force solution
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l        <- exprs ls,
                r        <- exprs rs,
                e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]
 
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- cli testing
main :: IO ()
main = do print (possibleExprs ns)
          print (validExprs ns)
          print (solutions' ns 765)
            where ns = [1,3,7,10,25,50]

-- optimization 1 -- combine generation and evaluation
type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                     lx      <- results ls,
                     ry      <- results rs,
                     res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

-- 1. See choices

-- 2. Note isChoise has typo, I will use it anyway :)
isChoise :: Eq a => [a] -> [a] -> Bool
isChoise [] ys      = True
isChoise (x:xs) ys  = (any (==x) ys) && (isChoise xs (removeFirstOcc x ys))

removeFirstOcc :: Eq a => a -> [a] -> [a]
removeFirstOcc x ys = (takeWhile (/= x) ys) ++ tail (dropWhile (/= x) ys)

-- 3. One has to be careful in generalizing split in this way since the base of the
--    recursion has to be changed as well. But after that one will go in an infinite
--    loop where exprs is called on the same list over and over if rs is empty. And thus
--    ls is the same as ns.

-- 4.
possibleExprs :: [Int] -> Int
possibleExprs xs = length (concat (map exprs (choices xs)))

validExprs :: [Int] -> Int
validExprs xs = length (concat (map eval (concat (map exprs (choices xs)))))


-- 5. See validness of Sub

-- 6. See Exp everywhere in the programm