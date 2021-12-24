module HW17 where

lsp :: (a -> Bool) -> [a] -> [a]
lsp p = reverse . foldl longer [] . scanl odot []
  where
    -- odot :: [a] -> a -> [a]
    -- ghci 觉得 odot 的 a 和 lsp 的 a 不一样，所以不注释掉就会报错。
    odot xs y | p y       = y : xs
              | otherwise = []
    longer :: [a] -> [a] -> [a]
    longer xs ys | length xs > length ys = xs
                 | otherwise             = ys

minimax :: [[Int]] -> Int
minimax = foldl odot maxBound 
  where
    odot :: Int -> [Int] -> Int
    odot a = foldl oplus minBound
      where
        oplus :: Int -> Int -> Int
        oplus b = max b . min a