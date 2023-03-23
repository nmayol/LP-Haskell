import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] _ = 0
degree ((x,y):xs) a
    | x == a || y == a = 1 + degree xs a
    | otherwise = degree xs a

degree' :: Eq a => [(a, a)] -> a -> Int
degree' x a = length(filter (\(f,s)-> f == a || s == a) x)

-- sort :: Ord a => [a] -> [a]
-- sort [] = []
-- sort (x:xs) = (sort (filter (< x) xs)) ++ [x] ++ (filter (>= x) xs)


neighbors :: Ord a => [(a, a)] -> a -> [a]
neighbors x y = sort (map (neighbour y) (filter (\(f,s)-> f == y || s == y) x))
    where
        neighbour :: Ord a => a -> (a,a) -> a
        neighbour y (f,s) 
            | f == y = s
            | otherwise = f
