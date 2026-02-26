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



isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [a] = []
pairs (x:y:xs) = (x, y) : pairs (y:xs)


prefixes :: [a] -> [[a]]
prefixex [] = [[]]
prefixes [a] = [[a]]
prefixes xs = prefixes(init xs) ++ [xs]


factors :: [a] -> [[a]]
factors [] = [[]]
factors [a] = [[a],[]]
factors xs = prefixes(xs) ++ factors(tail xs)


subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs xs = filterM (return[True,False])


isArithSerie :: (Eq a, Num a) => [a] -> Bool
isArithSerie [] = True
isArithSerie [_] = True
isArithSerie [_, _] = True
isArithSerie (x:y:z:xs) = (y - x == z - y) && isArithSerie (y:z:xs)

