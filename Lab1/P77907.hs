absValue :: Int -> Int
absValue x
    | x >= 0 = x
    | otherwise = -x


power :: Int -> Int -> Int
power _ 0 = 1
power x y = x * power x (y-1)

checkPrime :: Int -> Int -> Bool
checkPrime x 1 = True
checkPrime x y
    | x `mod` y == 0 = False
    | otherwise = checkPrime x (y-1)

isPrime :: Int -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = False
    | otherwise = checkPrime x (floor (sqrt (fromIntegral x)))

slowFib :: Int -> Int
slowFib x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = slowFib (x-1) + slowFib (x-2)

findFib :: Int -> Int -> Int -> Int -> Int
findFib a b i n
    | (i-1) == n = b
    | otherwise = findFib b (a+b) (i+1) n

quickFib :: Int -> Int
quickFib x
    | x == 0 = 0
    | otherwise = findFib 0 1 2 x

-- MAIN DE PROVA
-- main = putStrLn (show (quickFib 5))