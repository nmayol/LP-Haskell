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
average list = (sum list) div (myLength list)



-- MAIN DE PROVA
main = putStrLn (show (myMaximum [4,3,1,5,4,5,2]))