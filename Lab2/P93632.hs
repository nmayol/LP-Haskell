equalsZero :: Int -> Bool
equalsZero x = (x == 0)

eql :: [Int] -> [Int] -> Bool
eql x y
    | length x == length y = all(equalsZero) (zipWith (-) x y)
    | otherwise = False

prod :: [Int] -> Int
prod x = foldl (*) 1 x

prodOfEvens :: [Int] -> Int
prodOfEvens x = foldl (*) 1 (filter even x)


powersOf2 :: [Int]
powersOf2 = iterate (* 2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y =  foldl (+) 0 (zipWith (*) x y)
