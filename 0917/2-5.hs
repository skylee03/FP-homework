init :: [a] -> [a]
-- init xs = take (length xs - 1) xs
init xs = reverse (tail (reverse xs))