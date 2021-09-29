module HW4 where

-- Problem #1: 补全下列值的类型签名
val1 :: [Char]
val1 = ['a', 'b', 'c']

val2 :: (Char, Char, Char)
val2 = ('a', 'b', 'c')

val3 :: [(Bool, Char)]
val3 = [(False, '0'), (True, '1')]

val4 :: ([Bool], [Char])
val4 = ([False, True], ['0', '1'])

val5 :: [[a] -> [a]]
val5 = [tail, init, reverse]
-- End Problem #1

-- Problem #2: 补全下列函数的类型签名
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- End Problem #2

-- Problem #3: Int/Integer，show/read
-- Part #1: Int/Integer的区别
--   Int和Integer的区别是前者范围为[-2^63, 2^63-1]，后者没有限制。
--
--   Prelude> 2^63 :: Int
--   -9223372036854775808
--   Prelude> 2^63 :: Integer
--   9223372036854775808
-- End Part #1

-- Part #2: show/read的用法
--   Prelude> show 233
--   "233"
--   Prelude> read 233 :: Int
--   233
-- End Part #2
-- End Problem #3

-- Problem #4: Integral/Fractional
-- Part #1: Integral
--   Prelude> 666 `div` 233
--   2
--   Prelude> 666 `mod` 233
--   200
-- End Part #1

-- Part #2: Fractional
--   Prelude> 666 / 233
--   2.8583690987124464
--   Prelude> recip 233
--   4.291845493562232e-3
-- End Part #2
-- End Problem #3
