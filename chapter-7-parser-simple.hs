module ParserModule where
import Data.Char
--import Prelude hiding (return)

type Parser a = String -> [(a, String)]

item :: Parser Char
item = \ inp -> case inp of
                   [] -> []
                   (x:xs) -> [(x, xs)]

failure :: Parser Char
failure = \ inp -> []

char :: Parser Char
char = item

return' :: v -> Parser v
return' value = \ inp -> [(value, inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case p inp of
                        [] -> parse q inp -- first fails, proceed to 2
                        [(v, out)] -> [(v, out)] -- first pass, return

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

------------- Monad World
parse' :: Parser' a -> String -> [(a, String)]
parse' (P p) inp = p inp

newtype Parser' a = P (String -> [(a, String)])
instance Monad Parser' where
  return a = P (\ inp -> [(a, inp)])

  p >>= f = P (\ inp -> case (parse' p inp) of
                        [(v, out)] -> parse' (f v) out
                        [] -> [])


----- derived primitives world

-- parser satisfying a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
              then return x
              else failure
expr :: Parser Int
expr = do t <- item
          do char '+'
             e <- expr
             return (t + e)

          +++ return t

eval :: String -> Int
eval xs = fst (head(parse expr xs))

https://github.com/lpil/learning-haskell/blob/master/edx-fp101x/07-hw.hs

--}
