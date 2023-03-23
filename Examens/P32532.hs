
divisors :: Int -> [Int]
divisors a = [y | y <- [1..a],  a `mod` y == 0]

nbDivisors :: Int -> Int
nbDivisors =  length . divisors

moltCompost :: Int -> Bool
moltCompost x = all (==True) [dy < dx | y <- divisorsList, let dy = nbDivisors y]
    where
        dx = nbDivisors x
        divisorsList = [nbDivisors y | y <- [1..x-1]]

