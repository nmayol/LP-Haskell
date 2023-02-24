myLength :: [Int] -> Int
myLength [] = 0
myLength (x:list) = 1 + myLength list

findMaximum :: Int -> [Int] -> Int
findMaximum x [] = x
findMaximum x (y:list)
    | x > y = findMaximum x list
    | otherwise = findMaximum y list

myMaximum :: [Int] -> Int
myMaximum (x:list) = findMaximum x list


average :: [Int] -> Float
average list = s / m
    where
        s = fromIntegral (sum list) :: Float
        m = fromIntegral (myLength list) :: Float


buildPalindrome :: [Int] -> [Int]
buildPalindrome list = reverse list ++ list

remove :: [Int] -> [Int] -> [Int] 
remove [] _ = []
remove (x:xs) y
    | elem x y = remove xs y
    | otherwise = x : (remove xs y)

flatten :: [[Int]] -> [Int] 
flatten [] = []
flatten (x:xs) = x ++ flatten xs 

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    | x `mod` 2 == 0 = (x:(evens xs))
    | otherwise = evens xs

odds :: [Int] -> [Int]
odds [] = []
odds (x:xs)
    | x `mod` 2 /= 0 = (x:(odds xs))
    | otherwise = odds xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens list = (odds list, evens list)

-- void primeFactors(int n)
-- {
--   int c = 2;
--   while (n > 1) {
--     if (n % c == 0) {
--       printf("%d ", c);
--       n /= c;
--     }
--     else
--       c++;
--   }
-- }

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

primeDivisors :: Int -> [Int]
primeDivisors n = [divisor | divisor <-[2 .. n], mod n divisor == 0, isPrime divisor]
