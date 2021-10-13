module HW6 where

import Prelude hiding (and, concat, replicate, (!!), elem, filter, map)
import Data.Char
import Data.Bits

-- Problem #1: define prelude functions using recursions
and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:replicate (n-1) x

(!!) :: [a] -> Int -> a
(!!) xs 0     = head xs
(!!) (x:xs) n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem y (x:xs) = (y == x) || elem y xs
-- End Problem #1

-- Problem #2: merge ascending lists
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y
                        then x:merge xs (y:ys)
                        else y:merge (x:xs) ys
-- End Problem #2

-- Problem #3: merge sort
msort :: Ord a => [a] -> [a]
msort xs | length xs == 1 = xs
         | otherwise      = merge (msort $ take (n `div` 2) xs) (msort $ drop (n `div` 2) xs)
                              where n = length xs
-- End Problem #3

-- Problem #4: desugar list comprehension using map and filter
theExpr :: (a -> Bool) -> (a -> b) -> [a] -> [b]
-- theExpr p f xs = [f x | x <- xs, p x]
theExpr p f xs = map f $ filter p xs
-- End Problem #4

-- Problem #5: redefine map/filter with foldr
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x:xs) []
-- End Problem #5

-- Problem #6: error checking for binary string transmitter
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

decode :: [Bit] -> String
-- modify this line to add error checking
decode = map (chr . bin2int . (\xs -> if foldr xor 0 xs == 0 then tail xs else error "Error")) . chop

chop :: [Bit] -> [[Bit]]
chop [] = [] -- hint: not 'chop8' any more
chop xs = take 9 xs : chop (drop 9 xs)
-- End Problem #6
