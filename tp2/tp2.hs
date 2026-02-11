head' :: [a] -> a
head' [] = error "*** Exception: head: empty list"
head' (x:xs) = x


tail' :: [a] -> [a]
tail' [] = error "*** Exception: tail: empty list"
tail' (x:xs) = xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

interval :: Int -> [Int]
interval 0 = []
interval k = interval (k - 1) ++ [k]

interval' :: (Num a, Ord a) => a -> a -> [a]
interval' s k
  | s > k     = []
  | otherwise = s : interval' (s + 1) k
