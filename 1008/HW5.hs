module HW5 where
import Data.Char
import System.Posix.Terminal.ByteString (ControlCharacter(Interrupt))
import System.Posix.ByteString (ControlCharacter(Interrupt))

-- Problem #1: define safetail
-- Part #1: use a conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs
-- End Part #1

-- Part #2: use guarded equations
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs
-- End Part #2

-- Part #3: use pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs
-- End Part #3
-- End Problem #1

-- Problem #2: Luhn algorithm
luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4 = ((if x1 * 2 > 9 then x1 * 2 - 9 else x1 * 2) + x2
                 + (if x3 * 2 > 9 then x3 * 2 - 9 else x3 * 2) + x4) `mod` 10 == 0
-- End Problem #2

-- Problem #3: Caesar crack
encode :: Int -> String -> String 
encode n xs = [shift n x | x <- xs]

shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n c   = int2let ((let2int c + n) `mod` 26)

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char 
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crack :: String -> String
crack xs = encode (-factor) xs
  where factor = position (minimum chitab) chitab
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs
        freqs :: String -> [Float]
        freqs xs = [(fromIntegral $ count c xs :: Float) / (fromIntegral $ length xs :: Float) | c <- ['a'..'z']]
        chisqr :: [Float] -> [Float] -> Float
        chisqr os es = sum [(osi - esi) ^ 2 / esi | (osi, esi) <- zip os es]
        rotate :: Int -> [Float] -> [Float]
        rotate n xs = drop n xs ++ take n xs
        position :: Float -> [Float] -> Int
        position x xs | head xs == x = 0
                      | otherwise    = 1 + position x (tail xs)
        count :: Char -> String -> Int
        count c s | null s = 0
                  | otherwise = (if head s == c then 1 else 0) + count c (tail s)
-- End Problem #3

-- Problem #4: Pythagorean triples
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]
-- End Problem #4

-- Problem #5: perfect integers
perfects :: Int -> [Int]
perfects n = [x | x <- [6..n], sum (factors x) == x]
  where
    factors n = [x | x <- [1..n - 1], n `mod` x == 0]
-- End Problem #5

-- Problem #6: scalar product
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
-- End Problem #6
