last :: [a] -> a
last xs = head (drop (length xs - 1) xs)