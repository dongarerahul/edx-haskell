-- A Functor is any data type that defines how fmap [alias ($)]applies to it

-- Maybe is a Functor, Applicative Functor and Monad
instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap f Nothing = Nothing

-- Control.Applicative defines <*>, which knows how to apply a function wrapped in a context to a value wrapped in a context

--class (Functor f) => Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b
--
instance Applicative Maybe where
  pure = Just
  Nothing <*> = Nothing
  (Just f) <*> a = fmap f a

-- Monads are applicative functor supporting bind operation (>>=)
class Monad M a where
  return :: a -> M a
  (>>==) ::  M a -> (a -> M b) -> M b
  (>>) :: M a -> M b -> M b
  fail :: String -> M a
  fail message = error message

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing


