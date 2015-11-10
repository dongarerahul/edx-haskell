import Safe (readMay)
import Control.Applicative ((<$>))
import Control.Applicative ((<*>))
displayAge maybeAge =
  case maybeAge of
       Nothing -> putStrLn "You provided Invalid year"
       Just age -> putStrLn $ "In 2020, you will be : " ++ show age

calcAge futureYear birthYear = futureYear - birthYear

main = do
  putStrLn "Enter your birthyear: "
  birthYearString <- getLine

  putStrLn "Enter some year in future: "
  futureYearString <- getLine

-- Using monad power
--So if we can do such great stuff with functors and applicative functors, why do we need monads at all? The terse answer is context sensitivity: with a monad, you can make decisions on which processing path to follow based on previous results. With applicative functors, you have to always apply the same functions.

-- summary
-- <$> or fmap => functors: apply a function to a wrapped value using fmap or <$>
-- applicatives: apply a wrapped function to a wrapped value using <*> or liftA
-- monads: apply a function that returns a wrapped value, to a wrapped value using >>= or liftM

-- Short summary
-- <$> : functor (fmap)
-- <*> : applicatives (liftA)
-- >>= : monads (liftM)

  let maybeAge = do
        birthYear <- readMay birthYearString
        futureYear <- readMay futureYearString
        return $
          if futureYear > birthYear
             then calcAge futureYear birthYear
             else calcAge birthYear futureYear

  displayAge maybeAge

