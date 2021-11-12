module HW12 where

-- Problem #1: maximum segment sum, the straightforward version

segs :: (Num a, Ord a) => [[a]] -> [a] -> [[a]]
segs xss []     = xss
segs xss (y:ys) = xss ++ segs ([] : map (++ [y]) xss) ys

sums :: (Num a, Ord a) => [a] -> [a]
sums = map sum . segs [[]]

mss :: (Num a, Ord a) => [a] -> a
mss = maximum . sums
-- End Problem #1

-- Problem #2: maximum segment sum, the smart solution
solve :: (Num a, Ord a) => a -> [a] -> [a]
solve s []     = [s]
solve s (x:xs) = s : solve (max (s + x) 0) xs
mssSmart :: (Num a, Ord a) => [a] -> a
mssSmart = maximum . solve 0
-- End Problem #2
