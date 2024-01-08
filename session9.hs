import Control.Monad (replicateM)
hello :: IO ()
hello = do 
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name)

letter :: String -> IO ()
letter [] = return ()
letter (x:xs) = do
    putStrLn [x]
    letter xs

letter' :: String -> IO ()
letter' str = sequence_ [putStrLn [x] | x <- str]

hugorm :: IO()
hugorm = do
    putStrLn "How many numbers would you like to add?"
    input <- getLine
    let n = read input :: Int
    inputs <- replicateM n readLn
    let total = sum inputs
    putStrLn $ "The sum of the integers is: " ++ show total

sumInts :: Integer -> IO Integer
sumInts acc = do
    putStrLn "Enter an integer (0 to finish):"
    num <- readLn :: IO Integer
    if num == 0
        then return acc
        else sumInts (acc + num)

whileIO :: IO Integer -> (Integer -> Bool) -> (Integer -> Integer -> Integer) -> Integer -> IO Integer
whileIO getIO condF foldF acc = do
    val <- getIO
    if condF val
        then whileIO getIO condF foldF (foldF acc val)
        else pure acc
-- Can be used like this
-- totalSum <- whileIO (readLn :: IO Integer) (/= 0) (+) initialValue
-- getIO is the IO function we use
-- conF is the exit condition
-- foldF is the operation we apply to each number
-- acc is the initial value

-- Run the letter function with user input
letters :: IO ()
letters = do
    putStrLn "Enter a string:"
    input <- getLine
    letter input