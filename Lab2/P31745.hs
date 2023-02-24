myLength :: String -> Int
myLength list = foldl (+) 0 (map (const 1) list)

