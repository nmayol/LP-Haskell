

fizzBuzz :: [Either Int String]
fizzBuzz = map relate (iterate (+1) 0)
    where relate:: Int -> Either Int String
          relate x
            | (mod x 3 == 0) && (mod x 5 == 0) = Right "FizzBuzz"
            | (mod x 3 == 0) = Right "Fizz"
            | (mod x 5 == 0) = Right "Buzz"
            | otherwise = Left x
