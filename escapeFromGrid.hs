{-
Scrivi un programma in Haskell per simulare un semplice gioco di fuga su una griglia bidimensionale
rappresentata come una lista di liste di caratteri. Ogni cella della griglia può contenere uno
spazio vuoto ' ' su cui il giocatore può muoversi, un ostacolo 'X' che blocca il passaggio,
la posizione attuale del giocatore 'P', oppure l’uscita 'E'. Il programma dovrà includere
una funzione movePlayer che, data una sequenza di comandi di movimento
("U" per su, "D" per giù, "L" per sinistra, "R" per destra) e una griglia iniziale,
 restituisce la nuova griglia aggiornata con la posizione del giocatore dopo aver applicato i movimenti,
 ignorando quelli non validi. Inoltre, dovrà essere definita una funzione hasEscaped che verifica se il giocatore
 ha raggiunto l’uscita. L'obiettivo è modellare correttamente la logica del gioco e manipolare
 la struttura della griglia in modo funzionale.
 -}
import Data.List

type Grid = [[String]]

initializeGrid :: Grid
initializeGrid =
  [ [" ", "X", " ", "X"],
    [" ", " ", " ", " "],
    [" ", "X", "X", " "],
    ["P", "X", "E", " "]
  ]

localizePlayer :: Grid -> (Int, Int)
localizePlayer actualGrid = head [(r, c) | (r, row) <- zip [0 ..] actualGrid, (c, val) <- zip [0 ..] row, val == "P"]

localizeExit :: Grid -> (Int, Int)
localizeExit actualGrid = head [(r, c) | (r, row) <- zip [0 ..] actualGrid, (c, val) <- zip [0 ..] row, val == "E"]

checkGame :: (Int, Int) -> (Int, Int) -> Maybe Bool
checkGame (pR, pC) (eR, eC) =
  if abs (pR - eR) == 1 && abs (pC - eC) == 1 then Just True else Just False

movePlayer :: Grid -> (Int, Int) -> String -> Grid
movePlayer actualGrid (playerRow, playerColumn) move
  | move == "w" = moveTo actualGrid (playerRow, playerColumn) (-1, 0) 
  | move == "a" = moveTo actualGrid (playerRow, playerColumn) (0, -1) 
  | move == "s" = moveTo actualGrid (playerRow, playerColumn) (1, 0)
  | move == "d" = moveTo actualGrid (playerRow, playerColumn) (0, 1)
  | otherwise = actualGrid 

moveTo :: Grid -> (Int, Int) -> (Int, Int) -> Grid
moveTo grid (playerRow, playerColumn) (applyRow, applyColumn) =
  if emptyCell grid (playerRow, playerColumn) (applyRow, applyColumn)
    then
      let (newProw, newPcol) = (playerRow + applyRow, playerColumn + applyColumn)
          gridWithoutPlayer = modifyMatrix grid (playerRow, playerColumn) " "
          newGrid = modifyMatrix gridWithoutPlayer (newProw, newPcol) "P"
       in newGrid
    else
      grid

emptyCell :: Grid -> (Int, Int) -> (Int, Int) -> Bool
emptyCell grid (playerRow, playerColumn) (applyRow, applyColumn) =
  (grid !! (playerRow + applyRow) !! (playerColumn + applyColumn)) == " "

modifyMatrix :: Grid -> (Int, Int) -> String -> Grid
modifyMatrix grid (pR, pC) newChar =
  take pR grid ++ [modifyRow (grid !! pR) pC newChar] ++ drop (pR + 1) grid

modifyRow :: [String] -> Int -> String -> [String]
modifyRow row pC newChar =
  take pC row ++ [newChar] ++ drop (pC + 1) row

printMatrix :: Grid -> IO ()
printMatrix matrix = mapM_ (putStrLn . concatMap show) matrix

gameLoop :: Grid -> (Int, Int) -> (Int, Int) -> IO ()
gameLoop actualGrid (pR, pC) (eR, eC) = do
  case checkGame (pR, pC) (eR, eC) of
    Just True -> do
      putStrLn "Vittoria!"
    _ -> do
      putStrLn "Inserisci mossa da effettuare: [w,a,s,d]"
      input <- getLine
      -- let move = read input :: String
      let newGrid = movePlayer actualGrid (pR, pC) input
      let (newProw, newPcol) = localizePlayer newGrid
      printMatrix newGrid
      gameLoop newGrid (newProw, newPcol) (eR, eC)

main :: IO ()
main = do
  let grid = initializeGrid
  putStrLn "Benvenuto ad Escape from Grid!"
  printMatrix grid
  let (playerRow, playerCol) = localizePlayer grid
  let (victoryRow, victoryCol) = localizeExit grid
  gameLoop grid (playerRow, playerCol) (victoryRow, victoryCol)
