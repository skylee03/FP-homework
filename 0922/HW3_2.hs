-- 猜数字小游戏
-- 遇到问题：ghc-pkg: Couldn't open database
-- 解决方式：将 WSL 1 升级为 WSL 2
module Main where
  import System.Random

  main :: IO ()
  main = do
    x <- randomRIO (1, 100)
    let
      -- guess :: IO ()
      guess = do
        putStr "Please input an integer: "
        input <- getLine
        let x' = (read input :: Int)
        if x' == x then
          do putStrLn "You win!"
        else
          if x' < x then
            do putStrLn "Too small!"
               guess
          else
            do putStrLn "Too large!"
               guess
          
    guess