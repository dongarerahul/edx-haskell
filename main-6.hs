import Safe (readMay)

displayAge maybeAge =
  case maybeAge of
       Nothing -> putStrLn "You provided Invalid year"
       Just age -> putStrLn $ "In 2020, you will be : " ++ show age

main = do
  putStrLn "Enter your birthyear: "
  birthYearString <- getLine

  putStrLn "Enter some year in future: "
  futureYearString <- getLine

  let maybeAge =
        case readMay birthYearString of
           Nothing -> Nothing
           Just birthyear ->
             case readMay futureYearString of
                  Nothing -> Nothing
                  Just futureYear -> Just(futureYear - birthyear)

  displayAge maybeAge

