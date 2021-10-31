module HW10 where
import Control.Applicative
import Data.Char

-- Problem #1: Reader Monad
-- 因为 ((->) a) 在标准库中已经实现了 Monad，所以我们使用下面这个新定义的类型代替
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure f = Reader $ const f
  (<*>) (Reader f) (Reader g) = Reader $ \r -> f r (g r)

instance Monad (Reader r) where
  (>>=) (Reader g) f = Reader $ \r -> runReader (f (g r)) r
-- End Problem #1

-- Problem #2: Functor, Applicative, Monad
data Expr a
  = Var a
  | Val Int
  | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  fmap f (Var x) = Var $ f x
  fmap _ (Val x) = Val x
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  pure f = Var f
  Var f <*> x = fmap f x
  _ <*> _ = undefined

instance Monad Expr where
  Var x   >>= f = f x
  Val x   >>= _ = Val x
  Add x y >>= f = Add (x >>= f) (y >>= f)
-- End Problem #2

-- Problem #3: Why does factorising the expression grammar make the resulting parser more efficient?
-- 请把答案写在这里（注释里面）
-- End Problem #3

-- Problem #4: Extend the expression parser
newtype Parser a = P { parse :: String -> [(a,String)] }

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x, xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                             []         -> []
                             [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])
    
    -- (<*>) ::Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                              []         -> []
                              [(g, out)] -> parse (fmap g px) out)

three :: Parser (Char, Char)
three = -- g <$> item <*> item <*> item
        -- where g x y z = (x, z)
        do x <- item
           item
           z <- item
           return (x, z)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P $ const []
    
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            []         -> parse q inp
                            [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return $ t + e
           <|> do symbol "-"
                  e <- expr
                  return $ t - e
                <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return $ f * t
           <|> do symbol "/"
                  t <- term
                  return $ f `div` t
               <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> integer

eval :: String -> Int
eval = fst . head . parse expr

-- 我認爲 * 和 / 不應該右結合，不然 4 / 2 * 100 結果會是 0，但是這裏以題意爲準。
-- End Problem #4
