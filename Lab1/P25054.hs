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

primes :: Int -> Int -> [Int]
primes 1 _ = []
primes x y
    | x `mod` y == 0 && elem(y (primes ((x `div` y) (y)))) = primes ((x `div` y) (y))
    | x `mod` y == 0 = (y:primes (x `div` y) (y))
    | y == floor (sqrt (fromIntegral x)) = x:[]
    | otherwise = primes x (y+1)


primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors n = primes n 2
