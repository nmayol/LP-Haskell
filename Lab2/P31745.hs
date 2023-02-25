myLength :: String -> Int
myLength list = foldl (+) 0 (map (const 1) list)

isWord:: Char -> Bool
isWord x = (x /= ' ')

isNotWord:: Char -> Bool
isNotWord x = (x == ' ')


firstWord :: String -> String
firstWord x = takeWhile isWord y
    where 
        y = dropWhile isNotWord x


flatten :: [[Int]] -> [Int]
flatten x = foldl (++) [] x


countIn :: [[Int]] -> Int -> [Int]
countIn list y = map (length . filter (==y)) list

myReverse :: [Int] -> [Int]
myReverse a = foldr (\x y -> y++[x]) [] a