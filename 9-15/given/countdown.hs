-- arithmetic ops

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

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

-- choices
choices :: [a] -> [[a]]
choices = concat . map perms . subs

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
ops = [Add, Sub, Mul, Div]
 
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- cli testing
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

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


