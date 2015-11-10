import Safe (readMay)

displayAge maybeAge =
  case maybeAge of
       Nothing -> putStrLn "You provided Invalid year"
       Just age -> putStrLn $ "In 2020, you will be : " ++ show age

calcAge birthYear futureYear = futureYear - birthYear

main = do
  putStrLn "Enter your birthyear: "
  birthYearString <- getLine

  putStrLn "Enter some year in future: "
  futureYearString <- getLine

-- use partial function with functor map i.e. fmap
-- i.e. without use monad power (Maybe)
-- We are not using <- operator associated with IO monad
--
-- birthYear is kept as partial function

  let maybeAge = do
        birthYear <- fmap calcAge $ readMay birthYearString
        fmap birthYear $ readMay futureYearString

  displayAge maybeAge

