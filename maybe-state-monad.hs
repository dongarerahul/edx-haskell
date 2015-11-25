--import Prelude.Maybe hiding (Just, Nothing)

data Maybe' a = Just' a | Nothing'

instance Monad Maybe' where
  --return :: a -> Maybe a
  return x = Just' x

  --(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing' >>= _ = Nothing'
  Just' x >>= f = f x

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Monad Parser where
  return v = P (\ inp -> [(v, inp)])
  p >>= f = P (\ inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)
