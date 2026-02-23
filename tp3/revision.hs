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




