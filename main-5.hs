import Safe (readMay)

displayAge maybeAge =
  case maybeAge of
       Nothing -> putStrLn "You provided Invalid year"
       Just age -> putStrLn $ "In 2020, you will be : " ++ show age

calcAge n = 2020 - n

main = do
  putStrLn "Enter your birthyear: "
  yearString <- getLine

  -- instead of fmap, we can extract value out of
  -- maybe functor and apply calcAge function

  -- do notation available to only monads (special functors)
  -- do block help to extract value out of Maybe monads
  -- after manipulation, again package it back as monads
  let maybeAge =  do
      yearInteger <- readMay yearString
      return $ calcAge yearInteger

  displayAge maybeAge

