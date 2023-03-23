
bmi::Float -> Float -> Float
bmi m h = m / h2
    where
        h2 = h * h


interpretation::Float -> Float -> String
interpretation m h
    | x < 18 = "underweight"
    | x < 25 = "normal weight"
    | x < 30 = "overweight"
    | x < 40 = "obese"
    | otherwise = "severely obese"
    where
        x = bmi m h


main = do
    info <- getLine
    let phrase = words info
    if info /= ["*"]
       nom = read (info !! 0)
       m = read (info !! 1)
       h = read (info !! 2)
       putStrLn (nom ++ ": " ++ (interpretation m h))

