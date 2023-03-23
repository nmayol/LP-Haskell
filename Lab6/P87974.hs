

convert::String -> String
convert (x:xs)
    | x == 'A' || x == 'a' = "Hello!"
    | otherwise = "Bye!"



main = do
    x <- getLine
    putStrLn (convert x)

