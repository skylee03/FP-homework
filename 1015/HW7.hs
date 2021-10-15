module HW7 where

-- Problem #1: multiplies for natural numbers
data Nat = Zero | Succ Nat
  deriving (Show)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

multiplies :: Nat -> Nat -> Nat
multiplies Zero n     = Zero
multiplies (Succ m) n = add (multiplies m n) n
-- End Problem #1

-- Problem #2: folde for Exprs
data Expr
  = Val Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

-- try to figure out the suitable type yourself
folde :: (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Expr -> Int
folde val add mul (Val e)     = val e
folde val add mul (Add e1 e2) = add (folde val add mul e1) (folde val add mul e2)
folde val add mul (Mul e1 e2) = mul (folde val add mul e1) (folde val add mul e2)

-- eval = folde id (+) (*)
-- End Problem #2

-- Problem #3: recursive tree type
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show)
-- End Problem #3
