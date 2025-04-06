import Control.Monad (forever)
import Data.Array
import System.Random

createOrderedGrid :: Array Int Int
createOrderedGrid = listArray (0, 15) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0]

index :: (Int, Int) -> Int
index (r, c) = r *  + c  

getPosition :: Int -> (Int, Int)
getPosition i = (i `div` 4, i `mod` 4)

getCell :: Array Int Int -> (Int, Int) -> Int
getCell grid pos = grid ! (index, pos)

replaceCell :: Array Int Int -> (Int, Int) -> Int -> Array Int Int
replaceCell grid pos newVal = grid // [(index pos, newVal)]

applyMove :: Array Int Int -> (Int, Int) -> (Int, Int) -> Array Int Int
applyMove grid zeroPos direction =
  let newPos = findNewPos grid zeroPos direction
  in case newPos of
       Just pos -> replaceCell grid zeroPos (getCell grid pos)
       Nothing  -> grid

findNewPos :: Array Int Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findNewPos grid (r0, c0) (dr, dc) =
  let r = r0 + dr
      c = c0 + dc
   in if r >= 0 && r < 4 && c >= 0 && c < 4
        then Just (r, c)
        else Nothing

availableDirections :: [(Int, Int)]
availableDirections = [(1, 0), (0, -1), (-1, 0), (0, 1)]

finished :: Array Int Int -> Bool
finished grid = grid == createOrderedGrid

shuffleGrid :: Int -> StdGen -> Array Int Int
shuffleGrid moves rng =
  let directions = availableDirections
      zeroPos = getPosition 15 
      (movesArray, _) = buildMovesArray directions moves rng
  in foldl (\g move -> applyMove g zeroPos move) grid movesArray

buildMovesArray :: [(Int, Int)] -> Int -> StdGen -> ([(Int, Int)], StdGen)
buildMovesArray list moves rng = recursiveCopy list moves [] rng

recursiveCopy :: [(Int, Int)] -> Int -> [(Int, Int)] -> StdGen -> ([(Int, Int)], StdGen)
recursiveCopy _ 0 result rng = (result, rng)
recursiveCopy list n result rng =
  let (idx, rng') = randomR (0, length list - 1) rng
      elem = list !! idx
      newResult = elem : result
   in recursiveCopy list (n - 1) newResult rng'

printMatrix :: Array Int Int -> IO ()
printMatrix grid = mapM_ (putStrLn . unwords . map show . take 4 . drop (i*4)) [0..3]
  where i = index

gameLoop :: Array Int Int -> IO ()
gameLoop grid = do
  if finished grid then do
    putStrLn "Griglia completata!"
    printMatrix grid
  else do
    printMatrix grid
    putStrLn "Inserisci mossa: [0 (N) 1 (W) 2 (S) 3 (E)]"
    moveInput <- getLine
    let typeMove = read moveInput :: Int
    let zeroPos = getPosition 15
    let move = availableDirections !! typeMove
    let newGrid = applyMove grid zeroPos move
    gameLoop newGrid

main :: IO ()
main = do
  putStrLn "Inserisci il numero di mosse random: "
  input <- getLine
  let nMoves = read input :: Int
  let randomGen = mkStdGen 100
  let grid = shuffleGrid nMoves randomGen

  gameLoop grid