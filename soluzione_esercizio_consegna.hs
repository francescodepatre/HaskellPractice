import Data.List (sort, transpose)

parseInt = read :: String -> Int
parseRow = (map parseInt) . words
parseTab = (map parseRow) . lines

inner = tail . init

countRoofs = foldl (\ (m, c) x -> max (m,c) (x,c+1)) (0,0)

checkConstraint (x:xs) = x == 0 || x == (snd . countRoofs . init) xs

checkAllConstraints matrix =
    map checkConstraint $ ltr ++ rtl ++ ttb ++ btt
    where  
        ltr = inner matrix 
        rtl = map reverse ltr
        ttb = inner (transpose matrix)
        btt = map reverse ttb

checkRange xs = sort xs == [1 .. length xs]

checkAllRanges matrix = 
    map checkRange $ core ++ transpose core 
    where core = map inner (inner matrix)

main = do
    txt <- readFile "game3x3.txt"
    let matrix = parseTab txt

    let okRanges = checkAllRanges matrix 
    let okConstraints = checkAllConstraints matrix 
    let ok = and $ okRanges ++ okConstraints

    print ok 