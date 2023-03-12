
ones :: [Integer]
ones = repeat 1

nats :: [Integer]
nats = 0 : map (+1) nats


nats_no_zero :: [Integer]
nats_no_zero = 1 : map (+1) nats_no_zero

ints :: [Integer]
ints = tail ( concat ( map (\x -> [x, -x]) nats))

triangulars :: [Integer]
triangulars = scanl (+) 0 nats_no_zero

factorials :: [Integer]
factorials = scanl (*) 1 nats_no_zero

fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs)) 

checkPrime :: Integer -> Integer -> Bool
checkPrime x 1 = True
checkPrime x y
    | x `mod` y == 0 = False
    | otherwise = checkPrime x (y-1)

isPrime :: Integer -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = False
    | otherwise = checkPrime x (floor (sqrt (fromIntegral x)))

primes :: [Integer]
primes = filter isPrime nats


hammings :: [Integer]
hammings = tail ( concat ( map (\x y z -> 2*x * 3*y * 5*z) nats))


-- lookNsay :: [Integer]
-- tartaglia :: [[Integer]]
