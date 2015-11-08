import Safe (readMay)

displayAge maybeAge =
  case maybeAge of
       Nothing -> putStrLn "You provided Invalid year"
       Just age -> putStrLn $ "In 2020, you will be : " ++ show age

calcAge n = 2020 - n

main = do
  putStrLn "Enter your birthyear: "
  yearString <- getLine
  -- functor is a container with some value
  -- functor (Maybe) provides fmap i.e. functor map
  -- to modify its contents
  let maybeAge = fmap calcAge (readMay yearString)

  displayAge maybeAge

