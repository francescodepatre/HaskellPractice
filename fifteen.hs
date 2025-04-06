import Control.Monad (forever)
import Data.List
import System.Random

createOrderedGrid :: [[Int]]
createOrderedGrid =
  [ [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
    [13, 14, 15, 0]
  ]

finishedGrid :: [[Int]]
finishedGrid =
  [ [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
    [13, 14, 15, 0]
  ]

replaceCell :: Int -> a -> [a] -> [a]
replaceCell idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs

swapCells :: (Int, Int) -> (Int, Int) -> [[Int]] -> [[Int]]
swapCells (r1, c1) (r2, c2) originalGrid =
  let row1 = replaceCell c1 (originalGrid !! r2 !! c2) (originalGrid !! r1)
      row2 = replaceCell c2 (originalGrid !! r1 !! c1) (originalGrid !! r2)
   in replaceCell r1 row1 (replaceCell r2 row2 originalGrid)

findZero :: [[Int]] -> (Int, Int)
findZero fifGrid = head [(r, c) | (r, row) <- zip [0 ..] fifGrid, (c, val) <- zip [0 ..] row, val == 0]

availableDirections :: [(Int, Int)]
availableDirections = [(1, 0), (0, -1), (-1, 0), (0, 1)]

findCell :: [[Int]] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findCell fifGrid (r0, c0) (dr, dc) =
  let r = r0 + dr
      c = c0 + dc
   in if r >= 0 && r < 4 && c >= 0 && c < 4
        then Just (r, c)
        else Nothing

buildMovesArray :: [(Int, Int)] -> Int -> StdGen -> ([(Int, Int)], StdGen)
buildMovesArray list moves rng = recursiveCopy list moves [] rng

recursiveCopy :: [(Int, Int)] -> Int -> [(Int, Int)] -> StdGen -> ([(Int, Int)], StdGen)
recursiveCopy _ 0 result rng = (result, rng)
recursiveCopy list n result rng =
  let (idx, rng') = randomR (0, length list - 1) rng
      elem = list !! idx
      newResult = elem : result
   in recursiveCopy list (n - 1) newResult rng'

applyMove :: [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
applyMove fifGrid zeroPos direction =
  case findCell fifGrid zeroPos direction of
    Just newPos -> swapCells zeroPos newPos fifGrid
    Nothing -> fifGrid

finished :: [[Int]] -> Maybe Bool
finished grid =
  let completedGrid = finishedGrid
   in if grid == completedGrid then Just True else Nothing

shuffleGrid :: Int -> StdGen -> [[Int]]
shuffleGrid moves rng =
  let grid = createOrderedGrid
      zeroPos = findZero grid
      directions = availableDirections
      (movesArray, _) = buildMovesArray directions moves rng
   in foldl $ flip $ applyMove grid movesArray

printMatrix :: (Show a) => [[a]] -> IO ()
printMatrix matrix = mapM_ (putStrLn . unwords . map show) matrix

gameLoop :: [[Int]] -> IO ()
gameLoop grid = do
  case finished grid of
    Just True -> do
      putStrLn "Griglia completata!"
      printMatrix grid
    _ -> do
      printMatrix grid
      putStrLn "Inserisci mossa: [0 (N) 1 (W) 2 (S) 3 (E)]"
      moveInput <- getLine
      let typeMove = read moveInput :: Int
      let zeroPos = findZero grid
      let move = availableDirections !! typeMove
      let newGrid = applyMove grid zeroPos move
      gameLoop newGrid

main :: IO ()
main = do
  putStrLn "Inserisci il numero di mosse random: "
  input <- getLine
  let nMoves = read input :: Int
  let random = mkStdGen 100
  let grid = shuffleGrid nMoves random

  gameLoop grid
