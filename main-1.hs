import Safe (readMay)

main = do
  putStrLn "Enter your birthyear: "
  yearString <- getLine
  case readMay yearString of
       Nothing -> putStrLn "You provided Invalid year"
       Just year -> putStrLn $ "In 2020, you will be : " ++ show (2020 - year)

