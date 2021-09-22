module HW3_1 where

-- Problem #1: 写出 and 函数的三种其他的定义
and1 :: Bool -> Bool -> Bool
and1 True b = b
and1 _    _ = False

and2 :: Bool -> Bool -> Bool
and2 a b | a == True = b
         | otherwise = False

and3 :: Bool -> Bool -> Bool
and3 a b = if a == True then b else False
-- End Problem #1

-- Problem #2: 给出函数 div 的一种或多种定义
div1 :: Integer -> Integer -> Integer
div1 a b | a < b     = 0
         | otherwise = let
                         largest_doubling a b | b * 2 > a = b
                                              | otherwise = largest_doubling a (b * 2)
                         reduce a c n | c == b         = n
                                      | c `div` 2 <= a = reduce (a - c `div` 2) (c `div` 2) (n * 2 + 1)
                                      | otherwise      = reduce a (c `div` 2) (n * 2)
                         c = largest_doubling a b
                       in reduce (a - c) c 1
-- （丧失思考能力，把 From Mathematics to Generic Programming 这本书上的算法翻译了一遍，结果发现抄的是取模算法，舍不得删就留着了。）
mod1 :: Integer -> Integer -> Integer
mod1 a b | a < b     = a
         | otherwise = let
                         largest_doubling a b | b * 2 > a = b
                                              | otherwise = largest_doubling a (b * 2)
                         reduce a c | c == b         = a
                                    | c `div` 2 <= a = reduce (a - c `div` 2) (c `div` 2)
                                    | otherwise      = reduce a (c `div` 2)
                         c = largest_doubling a b
                       in reduce (a - c) c
-- End Problem #2

-- Problem #3: 写出阶乘函数的其他定义：
-- Part #1: 使用条件方程组
factGuard :: Integer -> Integer
factGuard n | n == 0    = 1
            | otherwise = n * factGuard (n - 1)
-- End Part #1

-- Part #2: 使用分支表达式
factBranch :: Integer -> Integer
factBranch n = if n == 0 then 1 else factGuard (n - 1)
-- End Part #2
-- End Problem #3
