{-
Extract a secret number, just once, between 1 and 90
The user has to guess it, repeatedly
For any try, provide a suggestion to the user
Tell if the guess is below or above the secret
In addition to the exercise
Count and tell the number of tries
Allow no more than 10 tries
-}

import System.Random

generateRandomNum :: IO Int
generateRandomNum = do
    gen <- newStdGen 
    let (num, _) = randomR (1, 90) gen  
    return num 

checkGuess :: Int -> Int -> Bool
checkGuess guess num = guess == num 

difference :: Int -> Int -> Int
difference num guess = num - guess

checkInfo :: Int -> String
checkInfo diff = if diff > 0
    then "Il numero da indovinare è maggiore" 
    else "Il numero da indovinare è minore"

initializeTries :: Int
initializeTries = 0

incTries :: Int -> Int
incTries tri = tri + 1

checkGame :: Int -> Bool
checkGame trN = trN <= 10

printTries :: Int -> IO ()
printTries tri = putStrLn("Numero di tentativi restanti: " ++ show (10 - tri))

game :: Int -> Int -> IO ()
game tries num = 
    if tries >= 10
        then putStrLn("Tentativi terminati!")
        else do
            putStrLn("Inserisci numero: ")
            input <- getLine
            let guessedNumber = read input :: Int 
            let chk = checkGuess guessedNumber num
            if chk
                then do
                    putStrLn("Indovinato!")
                    return ()
                else do
                    let diff = difference num guessedNumber
                    putStrLn ""
                    putStrLn (checkInfo diff)
                    let newTries = incTries tries
                    printTries newTries
                    game newTries num
     
main :: IO ()
main = do
    putStrLn("Benvenuto a Guess The Number!")
    num <- generateRandomNum
    let tries = initializeTries
    game tries num