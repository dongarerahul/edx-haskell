import Safe (readMay)

displayAge maybeAge =
  case maybeAge of
       Nothing -> putStrLn "You provided Invalid year"
       Just age -> putStrLn $ "In 2020, you will be : " ++ show age

calcAge n = 2020 - n

main = do
  putStrLn "Enter your birthyear: "
  yearString <- getLine
  let maybeAge =
        case readMay yearString of
          Nothing -> Nothing
          Just year -> Just (calcAge year)

  displayAge maybeAge

