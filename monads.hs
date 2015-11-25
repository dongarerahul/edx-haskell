{--

-- http://www.cs.nott.ac.uk/~pszgmh/monads

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b -- bind
    return :: a -> m a                 -- return while completing do block

3 laws that all monads should satisfy
 return a >>= f        = f a
 m >>= return          = m
 (m >>= f) >>= g       = m >>= (\x -> f x >>= g)

3 laws in do notation
 do x <- m
   return x
 =  do m

 do y <- return x
    f y
 = do f x

 do b <- do a <- m
    f a
 = do a <- m
      b <- f a
   g b
 = do a <- m
      do b <- f a
         g b



 auxilary operation defined in terms of bind i.e. >>=
 (>>) :: Monad m => m a -> m b -> m b
 m >> k                = m >>= (\_ -> k)

Monad provides uniform interface to 3 very different but fundamental ideas :
- FAILURE     : handling errors / exceptions' situation
- COLLECTIONS :
- EFFECTS     :
--}

{--
 sequence' [Just 1, Just 2] => Just [1, 2]
 sequence' [Just 1, Just 2, Nothing] => Nothing

 sequence' here allow us to collect series of computation results
 which can possibly fail or yield aggregated values if they all succeeded
--}

sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr mcons (return [])

mcons :: Monad m => m t -> m[t] -> m[t]
mcons p q = do
  x <- p
  y <- q
  return (x:y)


