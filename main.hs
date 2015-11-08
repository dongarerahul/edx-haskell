-- program fails if year entered from command-line is not a number
main = do
  putStrLn "Please Enter Your Birthyear: "
  year <- getLine
  putStrLn $ "In 2020, you will be : " ++ show (2020 - read year)

