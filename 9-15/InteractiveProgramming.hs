import System.IO
-- 1.
putStr' :: String -> IO ()
putStr' word = sequence_ [putChar c | c <- word]

-- 2. 
type Board = [Int]

showRow :: (Int, Int) -> String
showRow (a,b) = show a ++ ": " ++ concat (replicate b "* ") ++ "\n"

putBoardWithRowNumbers :: [(Int, Int)] -> IO ()
putBoardWithRowNumbers [x] = putStr (showRow x)
putBoardWithRowNumbers (x:xs) = do  putStr (showRow x)
                                    putBoardWithRowNumbers xs

putBoard :: Board -> IO ()
putBoard xs = putBoardWithRowNumbers (zip [1..] xs)

-- 3.
putBoard' :: Board -> IO ()
putBoard' b = sequence_ [putStr (showRow r) | r <- zip [1..] b]

-- 4.
adder :: IO ()
adder = do  putStr "how many numbers?"
            n <- getInt
            ns <- getIntList n
            putStr ("The total is " ++ show (sum ns))

getInt :: IO (Int)
getInt = readLn

getIntList :: Int -> IO ([Int])
getIntList 0 = return []
getIntList n = do m <- getInt
                  ms <- getIntList (n-1)
                  return (m:ms)

-- 5.
adder' :: IO ()
adder' = do  putStr "how many numbers?"
             n <- getInt
             ns <- sequence (replicate n getInt)
             putStr ("The total is " ++ show (sum ns))

-- 6. I do not get what the intention of this exercise is but I will ask you tomorrow
getCharHidden :: IO Char
getCharHidden = do  hSetEcho stdin False
                    x <- getChar
                    hSetEcho stdin True
                    return x

readChars :: IO ([Char])
readChars = do x <- getCharHidden
               if x == '\n' then
                  do putChar x
                     return []
               else
                  do putChar '-'
                     xs <- readChars
                     return (x:xs)

