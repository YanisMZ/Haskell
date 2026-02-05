abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n



myAverage :: Float -> Float -> Float -> Float
myAverage a b c = (a+b+c) / 3



myMin2 :: Int -> Int -> Int
myMin2 a b = min a b


myMin3 :: Int -> Int -> Int -> Int
myMin3 a b c = min (min a b) c


myAdd1 :: Int -> Int -> Int
myAdd1 x 0 = x
myAdd1 x y = myAdd1 (succ x) (pred y)


myMult :: Int -> Int -> Int
myMult x 0 = 0
myMult x y = myAdd1 x (myMult x (pred y))



myFact :: Integer -> Integer
myFact 0 = 1
myFact x = x * myFact(x - 1) 


myGCDEuclid :: Int -> Int -> Int
myGCDEuclid x y 
    | x == y = x
    | x > y = myGCDEuclid (x - y) y
    | otherwise = myGCDEuclid x (y - x)



myAverage2 :: Int -> Int -> Int -> Float
myAverage2 x y z = fromIntegral(x+y+z)/3



detectZero :: (Int -> Int) -> Int -> Int
detectZero f x0
  | f x0 == 0 = x0
  | otherwise = detectZero f (x0 + 1)

