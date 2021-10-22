module HW8 where

-- 为简便起见，我们不允许任何中间结果超出 2^32。
-- 这意味着可以提前对搜索进行剪枝：
-- 1. 任何结果均不允许超过 2^32。
-- 2. 任何指数均不允许超过 32。
-- 在大家的 64 位系统上，GHC 一般把 Int 实现为 64 位整数，因此
-- 只要严格执行上述剪枝策略，就不必担心运算溢出问题。

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Exp
  -- 提示：添加指数运算
  deriving Eq

data Expr
  = Val Int
  | App Op Expr Expr
  deriving Eq

-- 你需要完成下面的 solutions 函数

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y && x + y < 2 ^ 32
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1 && x * y < 2 ^ 32
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = y >= 0 && y /= 1 && y < 32 && x ^ y < 2 ^ 32

eval :: Expr -> [Int]
eval (Val x)      = [x | x > 0 && x < 2 ^ 32]
eval (App op l r) = [apply op x y | x <- eval l
                                  , y <- eval r
                                  , valid op x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
choices = concatMap perms . subs

values :: Expr -> [Int]
values (Val x) = [x]
values (App op l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:l, r) | (l, r) <- split xs]

type Result = (Expr, Int)

combine :: Result -> Result -> [Result]
combine (l, x) (r, y) = [(App o l r, apply o x y)
                            | o <- [Add, Sub, Mul, Div, Exp]
                            , valid o x y]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns
                  , l        <- results ls
                  , r        <- results rs
                  , res      <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = let
                   exprs = concat [results ns' | ns' <- choices ns]
                   sols = [e | (e, m) <- exprs, m == n]
                 in if sols /= [] then
                   sols
                 else
                   let
                     d = foldl (\res (e, m) -> min res $ abs (m - n)) (10^9) exprs
                   in [e | (e, m) <- exprs, abs (n - m) == d]

-- 下面是我们为 Expr 和 Op 提供的一个 Show 的实现
-- 这并不是本次作业必需的，但是在调试过程中可能会让表达式更易读
-- 这个实现使用了 showsPrec，有关它的细节你可以参考相关文档：
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Show.html#t:Show
-- 以及 Haskell 2010 Report 的 11.4 节：
-- https://www.haskell.org/onlinereport/haskell2010/haskellch11.html

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"
  -- 提示：指数运算可以显示为 x ^ y

instance Show Expr where
  showsPrec _ (Val n) = shows n
  showsPrec p (App op x y)
    = showParen (p > q)
    $ showsPrec q x . showChar ' ' . shows op
    . showChar ' ' . showsPrec (succ q) y
    where q = case op of
            Add -> 6; Sub -> 6
            Mul -> 7; Div -> 7
            Exp -> 8
            -- 提示：给出指数运算的优先级
            -- 可以参考Haskell定义的优先级（:info ^）
