properFactors :: Int -> [Int]
properFactors n = [x | x <- [1..n `div` 2], n `mod`x == 0]


perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (properFactors x) == x]


fizzBuzz :: [String]
fizzBuzz = [fb n | n <- [1..]]

fb :: Int -> String
fb n
  | n `mod` 15 == 0 = "fizzbuzz"
  | n `mod` 3  == 0 = "fizz"
  | n `mod` 5  == 0 = "buzz"
  | otherwise       = show n


altParity3 :: Int -> Int -> [(Int, Int, Int)]
altParity3 fromN toN =
  [ (x,y,z)
  | x <- [fromN..toN]
  , y <- [fromN..toN]
  , z <- [fromN..toN]
  , z < x
  , not (x == y && y == z)
  , not (even x == even y && even y == even z)
  ]

withAtLeastKLetters :: [String] -> Int -> [String]
withAtLeastKLetters xs k = [word | word <- xs , length word >= k]



withAtLeastKVowels :: [String] -> Int -> [String]
withAtLeastKVowels xss k = [word | word <- xss, countVowels word >= k]


countVowels :: String -> Int
countVowels word = length [c | c <- word, c `elem` "aeiouy"]


any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = if f x then True else any' f xs


any'' :: (a -> Bool) -> [a] -> Bool
any'' f xs = foldr(\x acc -> if f x then True else acc ) False xs


any''' :: (a -> Bool) -> [a] -> Bool
any''' f xs = foldr fb False xs
  where
    fb x acc = if f x then True else acc


group' :: (Eq a) => [a] -> [[a]]
group' xs = foldr (\x acc -> f x acc) [] xs
  where 
    f x [] = [[x]]
    f x (y:ys) 
        | x == head y = (x:y) : ys
        | otherwise   = [x] : y : ys



while :: (a -> Bool) -> (a -> a) -> a -> [a]
while p f x
  | p x       = x : while p f (f x)
  | otherwise = []





intersectI :: (Int, Int) -> (Int, Int) -> Bool
intersectI (a,b) (c,d) = a <= d && c <= b



isLeftSortedI :: [(Int, Int)] -> Bool
isLeftSortedI [] = True
isLeftSortedI [_] = True
isLeftSortedI ((a,_):(b,_):xs) =
  a <= b && isLeftSortedI ((b,0):xs)



sublists :: Int -> [a] -> [[a]]
sublists 0 _      = [[]]
sublists _ []     = []
sublists k (x:xs) =
  map (x:) (sublists (k-1) xs)
  ++ sublists k xs





cycle' :: [a] -> [a]
cycle' [] = error "cycle': empty list"
cycle' xs = xs ++ cycle' xs


cycle'' :: [a] -> [a]
cycle'' [] = error "cycle': empty list"
cycle'' xs = f xs
  where
    f [] = f xs
    f (y:ys) = y : f ys





sumOddsEvens :: [Int] -> (Int, Int)
sumOddsEvens xs = (sum odds, sum evens)
  where
    odds  = [x | x <- xs, odd x]
    evens = [x | x <- xs, even x]





oneOutOfEveryTwo :: [a] -> [a]
oneOutOfEveryTwo []       = []
oneOutOfEveryTwo [x]      = [x]
oneOutOfEveryTwo (x:_:xs) = x : oneOutOfEveryTwo xs




oneOutOfEveryK :: Int -> [a] -> [a]
oneOutOfEveryK k xs = [x | (x,i) <- zip xs [0..], i `mod` k == 0]



increasingPairs :: Int -> Int -> [(Int, Int)]
increasingPairs min max = [(x,y) | x <-[min..max], y <- [min..max], x<y,even (x+y)]