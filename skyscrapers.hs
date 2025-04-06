import Data.List
import System.IO

parseInt :: String -> Int
parseInt = read

parseRow :: String -> [Int]
parseRow = map parseInt . words

parseGrid :: String -> [[Int]]
parseGrid = map parseRow . lines

inner :: [a] -> [a]
inner = tail . init

innerGrid :: [[a]] -> [[a]]
innerGrid = map inner . inner

visibleCount :: [Int] -> Int
visibleCount = snd . foldl step (0, 0)
  where
    step (maxH, count) h
      | h > maxH = (h, count + 1)
      | otherwise = (maxH, count)

checkVisibilityConstraint :: [Int] -> Bool
checkVisibilityConstraint (clue : xs) = clue == 0 || clue == visibleCount (init xs)
checkVisibilityConstraint _ = True

checkAllVisibilityConstraints :: [[Int]] -> Bool
checkAllVisibilityConstraints grid =
  all checkVisibilityConstraint (ltr ++ rtl ++ ttb ++ btt)
  where
    ltr = innerGrid grid
    rtl = map reverse ltr
    ttb = innerGrid (transpose grid)
    btt = map reverse ttb

checkRange :: [Int] -> Bool
checkRange xs = sort xs == [1 .. length xs]

checkAllRanges :: [[Int]] -> Bool
checkAllRanges grid = all checkRange (core ++ transpose core)
  where
    core = innerGrid grid

validateGrid :: [[Int]] -> Bool
validateGrid grid = checkAllRanges grid && checkAllVisibilityConstraints grid

main :: IO ()
main = do
  contents <- readFile "skyscrapers-3x3.txt"
  let grid = parseGrid contents
  print $ validateGrid grid