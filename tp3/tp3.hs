elem' :: Eq a => a -> [a] -> Bool
elem' element liste = foldl verifier False liste
  where
    verifier dejaVu x = dejaVu || (x == element)

--elem1 :: a -> [a] -> Bool
--elem1 x' xs = foldl(\acc x -> acc || x== x') False xs --on peut retirer le xs etat reduction elem1 x'  = foldl(\acc x -> acc || x== x') False 

--elem2 :: a -> [a] -> Bool
--elem2 x' xs = foldl(\acc x -> acc || x == x') False xs


map1 :: (a -> b) -> [a] -> [b]
map1 x [] = []
map1 f(x :xs) = f x : map1 f xs


map2 :: (a -> b) -> [a] -> [b]
map2 f xs = [f x | x <- xs] 

map3 :: (a -> b) -> [a] -> [b]
map3 f = foldr (\x acc -> f x : acc) []

map4 :: (a -> b) -> [a] -> [b] 
map4 f xs = foldr s [] xs
    where
      s x acc = f x : acc



filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 p (x : xs)
  | p x = x : filter1 p xs
  | otherwise = filter1 p xs


takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs)
  | p x       = x : takeWhile1 p xs
  | otherwise = []

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p = foldr (\x acc -> if p x then x : acc else []) []


dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p(x:xs)
  | p x = dropWhile1 p xs
  | otherwise = x : xs


dropWhile2 :: ( a -> Bool) -> [a] -> [a]
dropWhile2 p = foldl op init 
    where 
      init = []
      op acc x = if null acc then if p x then [] else [x] else acc ++ [x]


zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 _ [] _ = []
zipWith1 _ _ [] = []
zipWith1 f (x : xs) (y : ys) = f x y : zipWith1 f xs ys



reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x : xs) = reverse1 xs ++ [x]


fibonacciSeq2 :: [Integer]
fibonacciSeq2 = 0 : 1 : zipWith (+) fibonacciSeq2 ( tail fibonacciSeq2)





distrib :: a -> [a] -> [[a]]
distrib x [] = [[x]]
distrib y (x:xs) = (y:x:xs) : map(x:) xss
  where 
    xss = distrib y xs


permutations1 :: [a] -> [[a]]
permutations1 [] = [[]]
permutations1 (x:xs) = concatMap(distrib x) (permutations1 xs)



shuffles :: [a] -> [a] -> [[a]]
shuffles [] xs = [xs]
shuffles xs [] = [xs]
shuffles (x:xs) (y:ys) = s1 ++ s2 
  where 
    s1 = map(x:) (shuffles xs (y:ys))
    s2 = map(y:) (shuffles (x:xs) ys)



unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
| p x = []
| otherwise = h x : unfold p h t (t x)



