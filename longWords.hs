longWords :: String -> [String]
longWords ws = filter (\xs -> length xs > 3) (words ws)