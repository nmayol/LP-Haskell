
myMap :: (a -> b) -> [a] -> [b]
myMap op list = [op x| x <- list]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond list = [x | x<-list, cond x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op list1 list2 = [op (fst x) (snd x)|x <- zip list1 list2]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l1 l2 = [(x,y)| x <- l1, y <- l2, (mod x y) == 0]

factors :: Int -> [Int]
factors x = [y | y <- [1..x] , (mod x y) == 0]
