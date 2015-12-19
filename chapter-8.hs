import System.IO

getChar' :: IO (Char, Char)
getChar' = do x <- getChar
              getChar
              y <- getChar
              return (x, y)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n'
                then return []
                else do xs <- getLine'
                        return (x:xs)

putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStr'' :: String -> IO()
putStr'' [] = return ()
putStr'' (x:xs) =  putChar x >> putStr'' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

putStrLn'' :: String -> IO ()
putStrLn'' [] =   putChar '\n'
putStrLn'' xs = putStr' xs >> putStrLn ""
--putStrLn'' xs = putStr' xs >>= \ x -> putChar '\n'
--putStrLn'' xs = putStr' xs >> putChar' '\n'
--putStrLn'' xs = putStr' xs >> putStr' "\n"
sequence' :: Monad m => [m a] -> m ()
sequence' [] = return ()
sequence' (m: ms) = (foldl (>>) m ms) >> return ()

sequence'' ms = foldl (>>) (return ()) ms
sequenc'' ms = foldr (>>=) (return ()) ms

sequence''' [] = return ()
sequence''' (m:ms) = m >> sequence''' ms

sequence'''' [] = return ()
sequence'''' (m:ms) = m >>= \ _ -> sequence''' ms


strLen :: IO ()
strLen = do putStr "Enter any string: "
            xs <- getLine
            putStr "Entered String had "
            putStr $ show $ length xs
            putStrLn' " characters."

---------- Hangman program : guess word
getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n'
                 then do putChar x
                         return []
                 else do putChar '-'
                         xs <- sgetLine
                         return (x:xs)

guess :: String -> IO ()
guess word = do putStr "> Can You Guess My Word ?\n"
                xs <- getLine
                if xs == word
                   then putStrLn "You Got It ! :)"
                   else do putStrLn (diff word xs)
                           guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]


