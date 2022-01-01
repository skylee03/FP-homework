module HW19 where

-- BMF3-2
tailsums :: Num a => [a] -> [a]
tailsums xs = let (t, s) = tup xs in reverse t
  where tup = foldr (\x (t, s) -> (s + x : t, s + x)) ([0], 0)
