module HW9 where

adder :: Int -> Int -> IO Int
adder 0 s = do return s
adder x s = do t <- fmap read getLine :: IO Int
               adder (x - 1) (s + t)

main :: IO ()
main = do putStr "How many numbers? "
          x <- fmap read getLine :: IO Int
          result <- adder x 0
          putStrLn ("The total is " ++ show result)
          return ()