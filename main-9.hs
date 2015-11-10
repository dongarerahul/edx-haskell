import Safe (readMay)
import Control.Applicative ((<$>))
import Control.Applicative ((<*>))
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

-- applicative functor
-- i.e. applying function (which is inside functor)
-- to value (which is also inside functor)

-- <$> uses a function which is not wrapped in a functor,
-- <*> uses a function which is wrapped up in a functor.
  let maybeAge = calcAge
        <$> readMay birthYearString
        <*> readMay futureYearString

  displayAge maybeAge

