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
flatten _ =  